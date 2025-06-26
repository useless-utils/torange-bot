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
  let conf = parseArgs args defaultConfig
  putStrLn $ "Username from CLI: " ++ username conf

  usernameEnv <- lookupEnv "TORANGE_BOT_REDDIT_USERNAME"
  secretEnv <- lookupEnv "TORANGE_BOT_REDDIT_SECRET"

  username <- case usernameEnv of
                Nothing -> die "ERROR: No username provided."
                Just x -> pure x

  print $ doesSecretHave2FA secretEnv

  putStrLn $ "Hi, " <> username

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
