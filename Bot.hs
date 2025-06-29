import Reddit.Actions.Post
import System.Environment
import System.Exit
import Data.Maybe
import Control.Applicative
import System.Directory
import System.FilePath

defaultConfig :: Config
defaultConfig = Config
  { username   = Nothing
  , secretFile = Nothing
  , twoFA      = Nothing
  }

data Config = Config
  { username   :: Maybe String
  , secretFile :: Maybe FilePath  -- NO secret via cli.
  , twoFA      :: Maybe String
  } deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  let confArgs = parseArgs args defaultConfig

  usernameEnv <- lookupEnv "TORANGE_BOT_REDDIT_USERNAME"
  secretEnv <- lookupEnv "TORANGE_BOT_REDDIT_SECRET"
  twoFAEnv <- lookupEnv "TORANGE_BOT_REDDIT_2FA"

  let conEnv = defaultConfig
        { username = usernameEnv
        , twoFA = twoFAEnv
        }

  let conf = defaultConfig
        { username = username confArgs <|> username conEnv
        , twoFA = twoFA confArgs <|> twoFA conEnv
        }

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

getConfigFile :: IO FilePath
getConfigFile = fmap (</> "torange-bot.conf") getConfigDir

doesConfigFileExist :: IO Bool
doesConfigFileExist = do
  doesFileExist =<< getConfigFile

createConfigDir :: IO (Either FilePath FilePath)
createConfigDir = do
  confDir <- getConfigDir
  if confDir == "."
    then pure $ Left confDir  -- using current directory as conf dir
    else do createDirectory confDir
            pure $ Right confDir
