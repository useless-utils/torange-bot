{-# LANGUAGE QuasiQuotes #-}

import Reddit.Actions.Post
import System.Environment
import System.Exit
import Data.Maybe
import Control.Applicative
import Data.Char
import Data.List
import System.IO
import Control.Monad
import Path.Parse
import Path.IO
import Data.Text qualified as T


defaultConfig :: Config
defaultConfig = Config
  { username   = Nothing
  , secretFile = Nothing
  , twoFA      = Nothing
  , configFile = Nothing
  }

data Config = Config
  { username   :: Maybe String
  , secretFile :: Maybe (Path Abs File)  -- NO secret via cli.
  , twoFA      :: Maybe String
  , configFile :: Maybe (Path Abs File)
  } deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  confArgs <- parseArgs args defaultConfig

  usernameEnv   <- lookupEnv          "TORANGE_BOT_REDDIT_USERNAME"
  secretEnv     <- lookupEnv          "TORANGE_BOT_REDDIT_SECRET"
  secretFileEnv <- lookupEnvFilePath  "TORANGE_BOT_REDDIT_SECRET_FILE"
  twoFAEnv      <- lookupEnv          "TORANGE_BOT_REDDIT_2FA"
  configFileEnv <- lookupEnvFilePath  "TORANGE_BOT_CONFIG_FILE"

  let confEnv = defaultConfig
                { username   = usernameEnv
                , twoFA      = twoFAEnv
                , configFile = configFileEnv
                , secretFile = secretFileEnv
                }

  defConfFile <- getDefConfigFile

  confFilePath <- do
    let isEnv = isJust configFileEnv
        isArgNotDef = configFile confArgs /= configFile defaultConfig

    case (isArgNotDef, isEnv) of
      (True, _) -> pure $ configFile confArgs
      (_, True) -> pure $ configFileEnv
      _         -> case defConfFile of
                     Left msg -> do hPutStrLn stderr msg
                                    die msg
                     Right path -> pure $ Just path

  hPutStrLn stderr $ "LOG: Using config file: " ++ (toFilePath $ fromJust confFilePath)
  confConfigFile <- parseConfigFile (fromJust confFilePath) defaultConfig

  let conf = defaultConfig
        { username = username confArgs <|> username confEnv <|> username confConfigFile
        , twoFA = twoFA confArgs <|> twoFA confEnv <|> twoFA confConfigFile
        , secretFile = secretFile confArgs <|> secretFile confEnv <|> secretFile confConfigFile
        , configFile = confFilePath
        }

  print conf
  let sf = toFilePath $ fromJust $ secretFile conf
  print sf
  -- doesFileExistOrDie sf $ "Secret file doesn't exist."


parseArgs :: [String] -> Config -> IO Config
parseArgs args conf =
  case args of
    ("--":_)               -> pure conf
    ("-u":a:xs)            -> parseArgs xs conf {username = Just a}
    ("--username":a:xs)    -> parseArgs xs conf {username = Just a}
    ("-s":a:xs)            -> do pa <- parseFilePath $ T.pack a
                                 parseArgs xs conf {secretFile = Just pa}
    ("--secret-file":a:xs) -> do pa <- parseFilePath $ T.pack a
                                 parseArgs xs conf {secretFile = Just pa}
    ("--2fa":a:xs)         -> parseArgs xs conf {twoFA = Just a}
    ("--config-file":a:xs) -> do pa <- parseFilePath $ T.pack a
                                 parseArgs xs conf {configFile = Just pa}
    ("-c":a:xs)            -> do pa <- parseFilePath $ T.pack a
                                 parseArgs xs conf {configFile = Just pa}
    (_:xs)                 -> parseArgs xs conf
    []                     -> pure conf

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
  confFileLines <- lines <$> (readFile $ toFilePath file)
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
                         then (kvToConfDo (fst x) (snd x) c') >>= \c'' -> kvToConf c'' xs
                         else kvToConf c' xs
    kvToConf c' [] = pure c'
    kvToConfDo :: String -> Maybe String -> Config -> IO Config
    kvToConfDo k v c'' = case k of
      "username"    -> pure c'' {username = v}
      "secret_file" -> case v of
                         Just v' -> do
                                  path <- parseFilePath (T.pack v')
                                  pure c'' {secretFile = Just path}
                         Nothing -> pure c'' {secretFile = Nothing}
      _             -> pure c''

isConfigKey :: String -> Bool
isConfigKey s = [s] `isInfixOf` [ "username"
                                , "secret_file"
                                ]

dropLeadingSpaces :: String -> String
dropLeadingSpaces s = dropWhile isSpace s

dropTrailingSpaces :: String -> String
dropTrailingSpaces s = dropWhileEnd isSpace s

dropInlineComment :: String -> String
dropInlineComment l = go l []
  where
    go :: String -> String -> String
    go (' ':'#':_) beforeC = beforeC
    go (x:xs) beforeC = go xs (beforeC <> [x])
    go _ beforeC = beforeC

doesFileExistOrDie :: Path b File -> String -> IO ()
doesFileExistOrDie f msg = doesFileExist f >>= \r -> unless r $ die msg

lookupEnvFilePath :: String -> IO (Maybe (Path Abs File))
lookupEnvFilePath env = lookupEnv env >>= \case
  Just p -> Just <$> parseFilePath (T.pack p)
  Nothing -> pure Nothing
