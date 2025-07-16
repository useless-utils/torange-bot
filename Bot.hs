{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors #-}


import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (hUserAgent)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy.Char8 qualified as CL

import System.Environment
import System.Exit
import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.Char
import Data.List
import System.IO
import Path.Parse
import Path.IO
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as T

import Text.JSON.Yocto
import Data.Map qualified as M
import Text.Printf
import Data.Bifunctor

defaultConfig :: Config
defaultConfig = Config
  { username         = Nothing
  , passwordFile     = Nothing
  , clientId         = Nothing
  , clientSecretFile = Nothing
  , twoFA            = Nothing
  , configFile       = Nothing
  }

data Config = Config
  { username         :: Maybe String
  , passwordFile     :: Maybe (Path Abs File)
  , clientId         :: Maybe String
  , clientSecretFile :: Maybe (Path Abs File)  -- NO secret via cli.
  , twoFA            :: Maybe String
  , configFile       :: Maybe (Path Abs File)
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
                     Just path -> pure $ Right path
                     Nothing -> pure $ Left "Couldn't get config file from args."
      (_, True) -> case confEnv.configFile of
                     Just path -> pure $ Right path
                     Nothing -> pure $ Left "Couldn't get config file from env. var."
      _         -> case defConfFile of
                     Left msg -> pure $ Left msg
                     Right path -> pure $ Right path
  vConfFilePath <- case confFilePath of
                     Right p -> pure p
                     Left msg -> do die msg

  confFile <- parseConfigFile vConfFilePath defaultConfig

  let conf = defaultConfig
        { username         = confArgs.username         <|> confEnv.username         <|> confFile.username
        , passwordFile     = confArgs.passwordFile     <|> confEnv.passwordFile     <|> confFile.passwordFile
        , clientId         = confArgs.clientId         <|> confEnv.clientId         <|> confFile.clientId
        , clientSecretFile = confArgs.clientSecretFile <|> confEnv.clientSecretFile <|> confFile.clientSecretFile
        , twoFA            = confArgs.twoFA            <|> confEnv.twoFA            <|> confFile.twoFA
        , configFile       = Nothing
        }

  passwordEnv     <- lookupEnv "TORANGE_BOT_REDDIT_PASSWORD"
  clientSecretEnv <- lookupEnv "TORANGE_BOT_REDDIT_CLIENT_SECRET"

  vUsername     <- fromConfOrDie      conf.username                         "ERROR: Username not provided."
  vPassword     <- fromFileOrEnvOrDie conf.passwordFile passwordEnv         "ERROR: Password not provided."
  vClientId     <- fromConfOrDie      conf.clientId                         "ERROR: Client ID not provided."
  vClientSecret <- fromFileOrEnvOrDie conf.clientSecretFile clientSecretEnv "ERROR: Client secret not provided."
  let vPass = vPassword ++ maybe [] (":" ++) conf.twoFA

  print conf  -- debug

  let userAgent = "torange-bot/0.1 by /u/torange-bot"
  -- get token
  req <- applyBasicAuth (C.pack vClientId) (C.pack vClientSecret)
         <$> parseRequest "POST https://www.reddit.com/api/v1/access_token"
  let req' = req { requestHeaders = (hUserAgent, C.pack userAgent) : req.requestHeaders }
      params :: [(C.ByteString, C.ByteString)]
      params = [ ( "grant_type", "password" )
               , ( "username", C.pack vUsername )
               , ( "password", C.pack vPass )
               ]
      accessTokenReq = urlEncodedBody params req'
  manager <- newManager tlsManagerSettings
  response <- httpLbs accessTokenReq manager
  let rBody = responseBody response
      decBody = decode $ CL.unpack rBody

  case responseError decBody of
    Just s -> do hPrintf stderr "ERROR: \"%s\", try again.\n" s
                 exitFailure
    Nothing -> pure ()

  accessToken <- case getAccessToken decBody of
                   Just token -> do
                     unless
                       (validateAccessToken token)
                       (die "ERROR: invalid token.")
                     pure $ C.pack token
                   Nothing -> die "ERROR: couldn't get token from response."

  reqSubmit <- applyBearerAuth accessToken <$> parseRequest "POST https://oauth.reddit.com/api/submit"
  postTextFromFile <- readFile $ toFilePath [relfile|body|]
  postTitleFromFile <- readFile $ toFilePath [relfile|title|]
  let reqSubmit' = reqSubmit { requestHeaders = (hUserAgent, C.pack userAgent) : reqSubmit.requestHeaders }
      reqSubmitData = [ ("api_type", "json")
                      , ("kind",     "link")
                      , ("sr",       toUtf8 postSubreddit)
                      , ("title",    toUtf8 postTitle)
                      , ("url",      toUtf8 postUrl)
                      -- , ("flair_id", toUtf8 postFlairId)
                      , ("text",     toUtf8 postText)
                      ]
      reqSubmitDataEncoded = urlEncodedBody reqSubmitData reqSubmit'
      postSubreddit = "LearnToReddit"
      postTitle = postTitleFromFile
      postUrl = ""
      postText = postTextFromFile

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

toUtf8 :: String -> ByteString
toUtf8 = TE.encodeUtf8 . T.pack

getAccessToken :: Value -> Maybe String
getAccessToken (Object o) =
  case M.lookup "access_token" o of
    Just (String token) -> Just token
    _ -> Nothing
getAccessToken _ = Nothing

responseError :: Value -> Maybe String
responseError (Object o) =
  case M.lookup "error" o of
    Just v -> case v of
                String s -> Just s
                _notStr  -> Nothing
    Nothing -> Nothing
responseError _ = Nothing

-- TODO Maybe also check for unusually max length?
validateAccessToken :: String -> Bool
validateAccessToken tok =
  not (null tok)
  && length tok > 50
  && all isValidChar tok
  where
    isValidChar c = isAlphaNum c || c `elem` ("-_." :: String)


parseArgs :: [String] -> Config -> IO Config
parseArgs args conf =
  case args of
    ("--":_)                      -> pure conf
    ("-u":a:xs)                   -> parseArgs xs conf {username = Just a}
    ("--username":a:xs)           -> parseArgs xs conf {username = Just a}
    ("-p":a:xs)                   -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {passwordFile = Just x}
    ("--password-file":a:xs)      -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {passwordFile = Just x}
    ("--client-id":a:xs)          -> parseArgs xs conf {clientId = Just a}
    ("--client-secret-file":a:xs) -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {clientSecretFile = Just x}
    ("--2fa":a:xs)                -> parseArgs xs conf {twoFA = Just a}
    ("-c":a:xs)                   -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {configFile = Just x}
    ("--config-file":a:xs)        -> parseFilePath (T.pack a) >>= parseArgs xs . \x -> conf {configFile = Just x}
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

  pure conf
    { username         = usernameEnv
    , passwordFile     = passwordFileEnv
    , clientId         = clientIdEnv
    , clientSecretFile = clientSecretFileEnv
    , twoFA            = twoFAEnv
    , configFile       = configFileEnv
    }

doesSecretHave2FA :: Maybe String -> Bool
doesSecretHave2FA (Just env) = go env
  where
    go (x:xs) = case x of
                  ':' -> True
                  _ -> go xs
    go [] = False
doesSecretHave2FA Nothing = False

getXdgConfigDir :: IO (Path Abs Dir)
getXdgConfigDir = getXdgDir XdgConfig $ Just [reldir|torange-bot|]

getConfigDir :: IO (Path Abs Dir)
getConfigDir = do
  doesIt <- doesDirExist =<< getXdgConfigDir
  if doesIt
  then getXdgConfigDir
  else getCurrentDir

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

parsePostFile :: Path Abs File -> Post -> IO Post
parsePostFile file post = do
  contents <- T.readFile (toFilePath file)
  let (postArgs, body) = T.breakOn "\n\n" contents
      tokenizePostArgs = map tokenizeHeaderLine $ T.lines postArgs
      bodyEnc = TE.encodeUtf8 $ T.strip body
      postArgs' = parsePostArgs tokenizePostArgs post

  pure postArgs' { title = postArgs'.title
                 , url = postArgs'.url
                 , subreddit = postArgs'.subreddit
                 , flairId = postArgs'.flairId
                 , body = Just bodyEnc
                 }
    where
      breakOnEq = T.breakOn "="
      stripVal = T.dropWhile (== '=') . T.strip
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
          -- TODO maybe implement body= as the start of body?
        else parsePostArgs xs p
      jSET = Just . TE.encodeUtf8 . snd
