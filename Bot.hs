{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors #-}


import Reddit
import Reddit.Actions.Post

import System.Environment
import System.Exit
import Data.Maybe
import Control.Applicative
import Data.Char
import Data.List
import System.IO
import Path.Parse
import Path.IO
import Data.Text qualified as T

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
                     Nothing -> pure $ Left "Couldn't get config file from args."
      _         -> case defConfFile of
                     Left msg -> pure $ Left msg
                     Right path -> pure $ Right path
  vConfFilePath <- case confFilePath of
                     Right p -> pure p
                     Left msg -> do die msg

  hPutStrLn stderr $ "LOG: Using config file: " ++ toFilePath vConfFilePath
  confFile <- parseConfigFile vConfFilePath defaultConfig

  let conf = defaultConfig
        { username         = confArgs.username         <|> confEnv.username         <|> confFile.username
        , passwordFile     = confArgs.passwordFile     <|> confEnv.passwordFile     <|> confFile.passwordFile
        , clientId         = confArgs.clientId         <|> confEnv.clientId         <|> confFile.clientId
        , clientSecretFile = confArgs.clientSecretFile <|> confEnv.clientSecretFile <|> confFile.clientSecretFile
        , twoFA            = confArgs.twoFA            <|> confEnv.twoFA            <|> confFile.twoFA
        , configFile       = Nothing
        }

  print conf

  passwordEnv     <- lookupEnv "TORANGE_BOT_REDDIT_PASSWORD"
  clientSecretEnv <- lookupEnv "TORANGE_BOT_REDDIT_CLIENT_SECRET"

  vUsername     <- fromConfOrDie      conf.username                         "ERROR: Username not provided."
  vPassword     <- fromFileOrEnvOrDie conf.passwordFile passwordEnv         "ERROR: Password not provided."
  vClientId     <- fromConfOrDie      conf.clientId                         "ERROR: Client ID not provided."
  vClientSecret <- fromFileOrEnvOrDie conf.clientSecretFile clientSecretEnv "ERROR: Client secret not provided."
  let vPass = vPassword ++ maybe [] (":" ++) conf.twoFA

  -- tests
  print vUsername
  print vPass
  print vClientId
  print vClientSecret
  where
    fromConfOrDie c errMsg = case c of
               Just a -> pure a
               Nothing -> die errMsg
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

-- TODO Account for empty value e.g. key= # comment
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
    kvToConfDo k v c'' = case k of
      "username"    -> pure c'' {username = v}
      "secret_file" -> case v of
                         Just v' | not (null v') -> do
                                     path <- parseFilePath (T.pack v')
                                     pure c'' {clientSecretFile = Just path}
                         _ -> pure c'' {clientSecretFile = Nothing}
      _             -> pure c''

isConfigKey :: String -> Bool
isConfigKey s = [s] `isInfixOf` [ "username"
                                , "secret_file"
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

