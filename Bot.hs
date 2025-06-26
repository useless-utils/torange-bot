import Reddit.Actions.Post
import System.Environment
import System.Exit

defaultConfig :: Config
defaultConfig = Config
  { username = []
  , secretFile = ".sec"
  , twoFA = []
  }

main :: IO ()
main = do
  args <- getArgs
  let confArgs = parseArgs args defaultConfig
  putStrLn $ "Username from CLI: " ++ username confArgs

  usernameEnv <- lookupEnv "TORANGE_BOT_REDDIT_USERNAME"
  secretEnv <- lookupEnv "TORANGE_BOT_REDDIT_SECRET"

  conf <- case usernameEnv of
               Nothing -> if username confArgs == []
                          then die "ERROR: No username provided."
                          else pure confArgs {username = username confArgs}
               Just x -> pure confArgs {username = x}

  print $ doesSecretHave2FA secretEnv

  putStrLn $ "Hi, " <> username conf

data Config = Config
  { username :: String
  , secretFile :: FilePath  -- NO secret via cli.
  , twoFA :: String
  } deriving (Show, Eq)

parseArgs :: [String] -> Config -> Config
parseArgs args conf =
  case args of
    ("--":_)               -> conf
    ("-u":a:xs)            -> parseArgs xs conf {username = a}
    ("--username":a:xs)    -> parseArgs xs conf {username = a}
    ("-s":a:xs)            -> parseArgs xs conf {secretFile = a}
    ("--secret-file":a:xs) -> parseArgs xs conf {secretFile = a}
    ("--2fa":a:xs)         -> parseArgs xs conf {twoFA = a}
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
