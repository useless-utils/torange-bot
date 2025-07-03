import Reddit.Actions.Post
import System.Environment
import System.Exit
import Data.Maybe
import Control.Applicative
import System.Directory
import System.FilePath
import Data.Char
import Data.List
import System.IO

defaultConfig :: Config
defaultConfig = Config
  { username   = Nothing
  , secretFile = Nothing
  , twoFA      = Nothing
  , configFile = "torange-bot.conf"
  }

data Config = Config
  { username   :: Maybe String
  , secretFile :: Maybe FilePath  -- NO secret via cli.
  , twoFA      :: Maybe String
  , configFile :: FilePath
  } deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  let confArgs = parseArgs args defaultConfig

  usernameEnv <- lookupEnv "TORANGE_BOT_REDDIT_USERNAME"
  secretEnv <- lookupEnv "TORANGE_BOT_REDDIT_SECRET"
  twoFAEnv <- lookupEnv "TORANGE_BOT_REDDIT_2FA"
  configFileEnv <- lookupEnv "TORANGE_BOT_CONFIG_FILE"

  let confEnv = defaultConfig
        { username = usernameEnv
        , twoFA = twoFAEnv
        }

  defConfFile <- getConfigFile
  confFilePath <-
    case configFileEnv of
      Just f -> do ret <- doesFileExist f
                   if ret
                     then pure f
                     else do die $ "ERROR: Could not open config file provided via "
                               ++ "environment variable \"TORANGE_BOT_CONFIG_FILE="
                               ++ f ++ "\". Aborting..."
      Nothing -> case defConfFile of
                   Left msg -> do hPutStrLn stderr msg
                                  die msg
                   Right path -> pure path

  hPutStrLn stderr $ "LOG: Using config file: " ++ confFilePath
  confConfigFile <- parseConfigFile confFilePath defaultConfig

  let conf = defaultConfig
        { username = username confArgs <|> username confEnv <|> username confConfigFile
        , twoFA = twoFA confArgs <|> twoFA confEnv <|> twoFA confConfigFile
        , secretFile = secretFile confArgs <|> secretFile confEnv <|> secretFile confConfigFile
        }

  print conf

  putStr "doesSecretHave2FA: "; print $ doesSecretHave2FA secretEnv

  putStrLn $ "Provided username: " <> (fromMaybe [] $ username conf)
  putStrLn $ "Provided 2FA: " <> (fromMaybe [] $ twoFA conf)
  -- putStrLn $ "Provided secretFile: " <> secretFile conf

parseArgs :: [String] -> Config -> Config
parseArgs args conf =
  case args of
    ("--":_)               -> conf
    ("-u":a:xs)            -> parseArgs xs conf {username = Just a}
    ("--username":a:xs)    -> parseArgs xs conf {username = Just a}
    ("-s":a:xs)            -> parseArgs xs conf {secretFile = Just a}
    ("--secret-file":a:xs) -> parseArgs xs conf {secretFile = Just a}
    ("--2fa":a:xs)         -> parseArgs xs conf {twoFA = Just a}
    (_:xs)                 -> parseArgs xs conf
    []                     -> conf

doesSecretHave2FA :: Maybe String -> Bool
doesSecretHave2FA (Just env) = go env
  where
    go (x:xs) = case x of
                  ':' -> True
                  _ -> go xs
    go [] = False
doesSecretHave2FA Nothing = False

getXdgConfigDir :: IO FilePath
getXdgConfigDir = getXdgDirectory XdgConfig "torange-bot"

getConfigDir :: IO FilePath
getConfigDir = do
  doesXdgConfDirExists <- doesDirectoryExist =<< getXdgConfigDir
  if doesXdgConfDirExists
    then getXdgConfigDir
    else makeRelativeToCurrentDirectory =<< getCurrentDirectory

getConfigFile :: IO (Either String FilePath)
getConfigFile = do
  file <- fmap (</> configFile defaultConfig) getConfigDir
  fileCheck <- doesFileExist file
  if fileCheck
    then pure $ Right file
    else pure $ Left $ "Default config file \"" ++ file ++ "\" doesn't exist."

doesConfigFileExist :: IO Bool
doesConfigFileExist = do
  confFile <- getConfigFile
  case confFile of
    Right _ -> pure True
    Left  _ -> pure False

createConfigDir :: IO (Either FilePath FilePath)
createConfigDir = do
  confDir <- getConfigDir
  if confDir == "."
    then pure $ Left confDir  -- using current directory as conf dir
    else do createDirectory confDir
            pure $ Right confDir

parseConfigFile :: FilePath -> Config -> IO Config
parseConfigFile file conf = do
  confFileLines <- lines <$> readFile file
  pure $ kvToConf conf $ mapMaybe parseLines confFileLines
  where
    parseLines line = case lineTokenise line [] [] of
      ([], []) -> Nothing
      (k,v) -> Just (k, Just v)
    lineTokenise (x:xs) key val
      | isSpace x = lineTokenise xs key val
      | x == '=' = (key, dropTrailingSpaces $ dropInlineComment $ dropLeadingSpaces xs)
      | otherwise = lineTokenise xs (key <> [x]) val
    lineTokenise [] _ _ = ([],[])

    kvToConf c' (x:xs) = if isConfigKey $ fst x
                         then kvToConf (kvToConfDo (fst x) (snd x) c') xs
                         else kvToConf c' xs
    kvToConf c' [] = c'
    kvToConfDo k v c'' = case k of
      "username"    -> c'' {username = v}
      "secret_file" -> c'' {secretFile = v}
      _             -> c''

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
