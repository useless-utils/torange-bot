{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}


import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (hUserAgent)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy.Char8 qualified as CL

import System.Environment
import System.Exit
import Data.Char
import Data.List ( dropWhileEnd, isInfixOf )
import Data.Maybe
import Control.Monad
import Control.Applicative
import System.IO
import Path.Parse
import Path.IO
import Data.Text (Text)
import Data.Text          qualified as T
import Data.Text.IO       qualified as T
import Data.Text.Encoding qualified as TE

import Text.JSON.Yocto qualified as Y
import Data.Map qualified as M
import Text.Printf
import Data.Bifunctor
import Data.Either

import Data.Time.Clock
import Data.Time.Format
import Text.Read ( readMaybe )

defaultConfig :: Config
defaultConfig = Config
  { username         = Nothing
  , passwordFile     = Nothing
  , clientId         = Nothing
  , clientSecretFile = Nothing
  , twoFA            = Nothing
  , configFile       = Nothing
  , postFile         = Nothing
  }

data Config = Config
  { username         :: Maybe String
  , passwordFile     :: Maybe (Path Abs File)
  , clientId         :: Maybe String
  , clientSecretFile :: Maybe (Path Abs File)  -- NO secret via cli.
  , twoFA            :: Maybe String
  , configFile       :: Maybe (Path Abs File)
  , postFile         :: Maybe (Path Abs File)
  } deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  confArgs <- parseArgs args defaultConfig

  confEnv <- parseEnv defaultConfig

  defConfFile <- getDefConfigFile
  confFilePath <- do
    let isEnv = isJust confEnv.configFile
        isArg = isJust confArgs.configFile
    case (isArg, isEnv) of
      (True, _) -> case confArgs.configFile of
                     Just path  -> pure $ Right path
                     Nothing    -> pure $ Left "Couldn't get config file from args."
      (_, True) -> case confEnv.configFile of
                     Just path  -> pure $ Right path
                     Nothing    -> pure $ Left "Couldn't get config file from env. var."
      _defToConfFile -> case defConfFile of
        Left msg   -> pure $ Left msg
        Right path -> pure $ Right path
  vConfFilePath <- case confFilePath of
                     Right p    -> pure p
                     Left msg   -> do die msg

  confFile <- parseConfigFile vConfFilePath defaultConfig

  let conf = defaultConfig
        { username         = confArgs.username         <|> confEnv.username         <|> confFile.username
        , passwordFile     = confArgs.passwordFile     <|> confEnv.passwordFile     <|> confFile.passwordFile
        , clientId         = confArgs.clientId         <|> confEnv.clientId         <|> confFile.clientId
        , clientSecretFile = confArgs.clientSecretFile <|> confEnv.clientSecretFile <|> confFile.clientSecretFile
        , twoFA            = confArgs.twoFA            <|> confEnv.twoFA            <|> confFile.twoFA
        , postFile         = confArgs.postFile         <|> confEnv.postFile
        , configFile       = Nothing
        }

  -- TODO check for existance and permissions
  when (isNothing conf.postFile) (die "ERROR: Post file not provided.")

  postData <- parsePostFile (fromJust conf.postFile) postDef
  when (isLeft postData) $ die "ERROR: Wrong post input data."
  let post = fromRight postDef postData

  passwordEnv     <- lookupEnv "TORANGE_BOT_REDDIT_PASSWORD"
  clientSecretEnv <- lookupEnv "TORANGE_BOT_REDDIT_CLIENT_SECRET"

  vUsername     <- fromConfOrDie      conf.username                         "ERROR: Username not provided."
  vPassword     <- fromFileOrEnvOrDie conf.passwordFile passwordEnv         "ERROR: Password not provided."
  vClientId     <- fromConfOrDie      conf.clientId                         "ERROR: Client ID not provided."
  vClientSecret <- fromFileOrEnvOrDie conf.clientSecretFile clientSecretEnv "ERROR: Client secret not provided."
  let vPass = vPassword ++ maybe [] (":" ++) conf.twoFA

  xdgAppDirEither <- createStateDir
  appDir <- case xdgAppDirEither of
                 Right p -> pure p
                 Left msg -> do
                   hPutStr stderr (msg <> " Defaulting to app dir: ")
                   appDir <- getAppUserDataDir "torange-bot"
                   ensureDir appDir
                   hPutStrLn stderr $ toFilePath appDir
                   pure appDir
  let accessFile = appDir </> [relfile|access|]

  isAccessFileReadable <- (&&) <$> doesFileExist accessFile <*> ((.readable) <$> getPermissions accessFile)
  isAccessFileValid <- isAccessFileStillValid accessFile

  -- get token
  let userAgent = "torange-bot/0.1 by /u/torange-bot"
  manager <- newManager tlsManagerSettings

  -- TODO when reading the existing access file check for saved_at?
  responseBodyDecoded <-
    if isAccessFileReadable && isAccessFileValid
    then Y.decode <$> readFile (toFilePath accessFile)
    else httpAccessTokenRequest manager vClientId vClientSecret vUsername vPass userAgent
         >>= \case
         Left (b, e) -> do hPrintf stderr "HTTP ERROR: \"%s\", try again.\n" e
                           pure b
         Right b -> pure b

  unless (isAccessFileReadable && isAccessFileValid)
    $ C.writeFile (toFilePath accessFile) (toUtf8 $ Y.encode responseBodyDecoded)

  accessToken <- case getAccessToken responseBodyDecoded of
                   Just token -> do
                     unless
                       (validateAccessToken token)
                       (die "ERROR: invalid token.")
                     pure $ toUtf8 token
                   Nothing -> die "ERROR: couldn't get token from response."

  reqSubmit <- applyBearerAuth accessToken <$> parseRequest "POST https://oauth.reddit.com/api/submit"

  let reqSubmit' = reqSubmit { requestHeaders = (hUserAgent, toUtf8 userAgent) : reqSubmit.requestHeaders }
      reqSubmitData = catMaybes $
        [ Just ("api_type", "json")
        , Just ("kind",     "link")
        ] <> postParams post
      reqSubmitDataEncoded = urlEncodedBody reqSubmitData reqSubmit'

  responseSubmit <- httpLbs reqSubmitDataEncoded manager
  print responseSubmit -- debug
  putStrLn $ CL.unpack $ responseBody responseSubmit

  where
    fromConfOrDie :: Maybe String -> String -> IO String
    fromConfOrDie c errMsg = case c of
               Just a -> pure a
               Nothing -> die errMsg
    fromFileOrEnvOrDie :: Maybe (Path b t) -> Maybe String -> String -> IO String
    fromFileOrEnvOrDie mFile mEnv errMsg = do
      -- expects to be the sole contents of the file:
      contents <- case mFile of
                        Just f -> readFile $ toFilePath f
                        Nothing -> pure ""

      case mEnv of
        Just p -> pure p
        Nothing -> if null contents
                   then die errMsg
                   else pure contents

-- http
httpAccessTokenRequest
  :: Manager
  -> String
  -> String
  -> String
  -> String
  -> String
  -> IO (Either (Y.Value, String) Y.Value)
httpAccessTokenRequest manager cid csec user pass ua = do
  req <- applyBasicAuth (toUtf8 cid) (toUtf8 csec)
         <$> parseRequest "POST https://www.reddit.com/api/v1/access_token"
  let req' = req { requestHeaders = (hUserAgent, toUtf8 ua) : req.requestHeaders }
      params :: [(C.ByteString, C.ByteString)]
      params = [ ( "grant_type", "password" )
               , ( "username", toUtf8 user )
               , ( "password", toUtf8 pass )
               ]
      accessTokenReq = urlEncodedBody params req'

  response <- httpLbs accessTokenReq manager
  let rBody = responseBody response
      rBodyDec = Y.decode $ CL.unpack rBody
  rBodyDecT <- insertTimestamp rBodyDec >>= \case
    Right b -> pure b
    -- TODO propagate this higher!
    Left b -> do hPutStrLn stderr "WARNING: Couldn't add timestamp to access_token!"
                 pure b

  case httpResponseError rBodyDecT of
    Just s -> pure $ Left (rBodyDecT, s)
    Nothing -> pure $ Right rBodyDecT

httpResponseError :: Y.Value -> Maybe String
httpResponseError (Y.Object o) =
  case M.lookup "error" o of
    Just v -> case v of
                Y.String s -> Just s
                _notStr  -> Nothing
    Nothing -> Nothing
httpResponseError _ = Nothing

-- encoding
toUtf8 :: String -> ByteString
toUtf8 = TE.encodeUtf8 . T.pack


-- access file
getAccessToken :: Y.Value -> Maybe String
getAccessToken (Y.Object o) =
  case M.lookup "access_token" o of
    Just (Y.String token) -> Just token
    _ -> Nothing
getAccessToken _ = Nothing

isAccessFileStillValid :: Path Abs File -> IO Bool
isAccessFileStillValid f = do
  c <- readFile (toFilePath f)
  let dc = Y.decode c
  case fromObj dc of
    Left _ -> pure False
    Right o ->
      case M.lookup "saved_at" o of
        Nothing -> pure False
        Just v -> do
          let savedAt (Y.String s) = readMaybe s :: Maybe UTCTime
              savedAt _ = Nothing
          case savedAt v of
            Nothing -> pure False
            Just st ->
              (addUTCTime (secondsToNominalDiffTime (60*60*24)) st >) <$> getCurrentTime

insertTimestamp :: Y.Value -> IO (Either Y.Value Y.Value)
insertTimestamp (Y.Object a) = do
  t <- getCurrentTime
  pure $ Right $ Y.Object $ M.insert "saved_at" (Y.String $ show t) a
insertTimestamp a = pure $ Left a

-- TODO Maybe also check for unusually max length?
validateAccessToken :: String -> Bool
validateAccessToken tok =
  not (null tok)
  && length tok > 50
  && all isValidChar tok
  where
    isValidChar c = isAlphaNum c || c `elem` ("-_." :: String)


-- args
parseArgs :: [String] -> Config -> IO Config
parseArgs args conf =
  case args of
    ("--":_)                      -> pure conf
    ("-u":a:xs)                   -> parseArgs xs conf {username = Just a}
    ("--username":a:xs)           -> parseArgs xs conf {username = Just a}
    ("-P":a:xs)                   -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {passwordFile = Just x}
    ("--password-file":a:xs)      -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {passwordFile = Just x}
    ("--client-id":a:xs)          -> parseArgs xs conf {clientId = Just a}
    ("--client-secret-file":a:xs) -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {clientSecretFile = Just x}
    ("--2fa":a:xs)                -> parseArgs xs conf {twoFA = Just a}
    ("-c":a:xs)                   -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {configFile = Just x}
    ("--config-file":a:xs)        -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {configFile = Just x}
    ("-p":a:xs)                   -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {postFile = Just x}
    ("--post-file":a:xs)          -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {postFile = Just x}
    (_:xs)                        -> parseArgs xs conf
    []                            -> pure conf

parseEnv :: Config -> IO Config
parseEnv conf = do
  usernameEnv         <- lookupEnv         "TORANGE_BOT_REDDIT_USERNAME"
  passwordFileEnv     <- lookupEnvFilePath "TORANGE_BOT_REDDIT_PASSWORD_FILE"
  clientIdEnv         <- lookupEnv         "TORANGE_BOT_REDDIT_CLIENT_ID"
  clientSecretFileEnv <- lookupEnvFilePath "TORANGE_BOT_REDDIT_CLIENT_SECRET_FILE"
  twoFAEnv            <- lookupEnv         "TORANGE_BOT_REDDIT_2FA"
  configFileEnv       <- lookupEnvFilePath "TORANGE_BOT_CONFIG_FILE"
  postFileEnv         <- lookupEnvFilePath "TORANGE_BOT_POST_FILE"

  pure conf
    { username         = usernameEnv
    , passwordFile     = passwordFileEnv
    , clientId         = clientIdEnv
    , clientSecretFile = clientSecretFileEnv
    , twoFA            = twoFAEnv
    , configFile       = configFileEnv
    , postFile         = postFileEnv
    }

doesSecretHave2FA :: Maybe String -> Bool
doesSecretHave2FA (Just env) = go env
  where
    go (x:xs) = case x of
                  ':' -> True
                  _ -> go xs
    go [] = False
doesSecretHave2FA Nothing = False


-- config file and dir
getXdgConfigDir :: IO (Path Abs Dir)
getXdgConfigDir = getXdgDir XdgConfig $ Just [reldir|torange-bot|]

getConfigDir :: IO (Path Abs Dir)
getConfigDir = do
  doesIt <- doesDirExist =<< getXdgConfigDir
  if doesIt
  then getXdgConfigDir
  else getCurrentDir

data IsXdgState p t = Exist p  -- ^ Existing path
                    | MissingTarget t  -- ^ The app dir inside 'XdgState'
                    | MissingBase  -- ^ 'XdgState' dir itself

getXdgStateDir :: IO (Path Abs Dir)
getXdgStateDir = getXdgDir XdgState $ Just [reldir|torange-bot|]

isXdgStateDir :: IO (IsXdgState (Path Abs Dir) (Path Abs Dir))
isXdgStateDir = do
  xdg <- getXdgStateDir
  isXdg <- doesDirExist xdg
  isBaseXdgState <- doesDirExist =<< getXdgDir XdgState Nothing
  if isBaseXdgState
    then if isXdg
         then pure $ Exist xdg
         else pure $ MissingTarget xdg
    else pure MissingBase

createStateDir :: IO (Either String (Path Abs Dir))
createStateDir = do
  xdg <- isXdgStateDir
  case xdg of
    Exist p -> do pure $ Right p
    MissingTarget t -> do ensureDir t
                          Right <$> getXdgStateDir
    MissingBase -> pure $ Left "Base XdgState directory not found."

getDefConfigFile :: IO (Either String (Path Abs File))
getDefConfigFile = do
  file <- makeAbsolute [relfile|torange-bot.conf|]
  fileCheck <- doesFileExist file
  if fileCheck
    then pure $ Right file
    else pure $ Left $ "Default config file \"" ++ toFilePath file ++ "\" doesn't exist."

parseConfigFile :: Path Abs File -> Config -> IO Config
parseConfigFile file conf = do
  confFileLines <- lines <$> readFile (toFilePath file)
  kvToConf conf $ mapMaybe parseLines confFileLines
  where
    parseLines line = case lineTokenise line [] [] of
      ([], []) -> Nothing
      (k,v) -> Just (k, Just v)
    lineTokenise :: String -> String -> String -> (String, String)
    lineTokenise (x:xs) key val
      | isSpace x = lineTokenise xs key val
      | x == '=' = (key, dropTrailingSpaces $ dropInlineComment $ dropLeadingSpaces xs)
      | otherwise = lineTokenise xs (key <> [x]) val
    lineTokenise [] _ _ = ([],[])
    kvToConf :: Config -> [(String, Maybe String)] -> IO Config
    kvToConf c' (x:xs) = if isConfigKey $ fst x
                         then uncurry kvToConfDo x c' >>= \c'' -> kvToConf c'' xs
                         else kvToConf c' xs
    kvToConf c' [] = pure c'
    kvToConfDo :: String -> Maybe String -> Config -> IO Config
    kvToConfDo k v c = case k of
      "username"           -> pure c {username = v}
      "password_file"      -> vCheckParsePath (\p c' -> c' {passwordFile = p}) v c
      "client_id"          -> pure c {clientId = v}
      "client_secret_file" -> vCheckParsePath (\p c' -> c' {clientSecretFile = p}) v c
      _                    -> pure c
    vCheckParsePath :: (Maybe (Path Abs File) -> Config -> Config) -> Maybe String -> Config -> IO Config
    vCheckParsePath setter v c = do
      p <- traverse (parseFilePath . T.pack) v
      pure $ setter p c

isConfigKey :: String -> Bool
isConfigKey s = [s] `isInfixOf` [ "username"
                                , "password_file"
                                , "client_id"
                                , "client_secret_file"
                                ]

dropLeadingSpaces :: String -> String
dropLeadingSpaces = dropWhile isSpace

dropTrailingSpaces :: String -> String
dropTrailingSpaces = dropWhileEnd isSpace

dropInlineComment :: String -> String
dropInlineComment l = go l []
  where
    go :: String -> String -> String
    go (' ':'#':_) beforeC = beforeC
    go (x:xs) beforeC = go xs (beforeC <> [x])
    go _ beforeC = beforeC

lookupEnvFilePath :: String -> IO (Maybe (Path Abs File))
lookupEnvFilePath env = lookupEnv env >>= \case
  Just p -> Just <$> parseFilePath (T.pack p)
  Nothing -> pure Nothing


-- post file
data Post = Link
  { title     :: Maybe ByteString
  , url       :: Maybe ByteString
  , subreddit :: Maybe ByteString
  , body      :: Maybe ByteString
  , flairId   :: Maybe ByteString
  } deriving (Eq, Show)

postDef :: Post
postDef       = Link
  { title     = Nothing
  , url       = Nothing
  , subreddit = Nothing
  , body      = Nothing
  , flairId   = Nothing
  }

parsePostFile :: Path Abs File -> Post -> IO (Either Post Post)
parsePostFile file post = do
  contents <- T.readFile (toFilePath file)
  let (postArgs, body) = T.breakOn "\n\n" contents
      tokenizePostArgs = map tokenizeHeaderLine $ T.lines postArgs
      bodyEnc = TE.encodeUtf8 $ T.strip body
      post' = parsePostArgs tokenizePostArgs post
      p = post' { title     = post'.title
                , url       = post'.url
                , subreddit = post'.subreddit
                , flairId   = post'.flairId
                , body      = if C.null bodyEnc
                              then Nothing
                              else Just bodyEnc
                }

  if all isJust [p.title, p.url, p.subreddit]
    then pure $ Right p
    else pure $ Left p
    where
      breakOnEq = T.breakOn "="
      stripVal = T.strip . T.dropWhile (== '=')
      tokenizeHeaderLine = bimap T.strip stripVal . breakOnEq
      isPostKey k = any (k `T.isPrefixOf`) postHeaderKeys
      postHeaderKeys =
        [ "title", "url", "link", "sr", "subreddit", "flair_id", "flairId" ]
      parsePostArgs :: [(Text, Text)] -> Post -> Post
      parsePostArgs [] p = p
      parsePostArgs (x:xs) p =
        if isPostKey $ fst x
        then case fst x of
          "title"                          -> parsePostArgs xs p {title     = jSET x}
          k | k `elem` ["url", "link"]     -> parsePostArgs xs p {url       = jSET x}
          k | k `elem` ["subreddit", "sr"] -> parsePostArgs xs p {subreddit = jSET x}
          "flair_id"                       -> parsePostArgs xs p {flairId   = jSET x}
          _ -> parsePostArgs xs p
        else parsePostArgs xs p
      jSET :: (Text, Text) -> Maybe ByteString
      jSET v | T.null $ snd v = Nothing
             | otherwise      = (Just . TE.encodeUtf8 . snd) v

postParams :: Post -> [Maybe (ByteString, ByteString)]
postParams post =
  [ mayParam "title"    post.title
  , mayParam "url"      post.url
  , mayParam "sr"       post.subreddit
  , mayParam "text"     post.body
  , mayParam "flair_id" post.flairId
  ]
  where
    mayParam :: ByteString -> Maybe ByteString -> Maybe (ByteString, ByteString)
    mayParam "title"    (Just x) = Just ("title"    , x)
    mayParam "url"      (Just x) = Just ("url"      , x)
    mayParam "sr"       (Just x) = Just ("sr"       , x)
    mayParam "text"     (Just x) = Just ("text"     , x)
    mayParam "flair_id" (Just x) = Just ("flair_id" , x)
    mayParam _ (Just "") = Nothing
    mayParam _ (Just _)  = Nothing
    mayParam _ Nothing   = Nothing


-- yocto json
fromObj :: Y.Value -> Either (M.Map String Y.Value) (M.Map String Y.Value)
fromObj (Y.Object o) = Right o
fromObj _ = Left M.empty
