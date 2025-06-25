import Reddit.Actions.Post
import System.Environment
import System.Exit

main :: IO ()
main = do
  usernameEnv <- lookupEnv "TORANGE_BOT_REDDIT_USERNAME"
  secretEnv <- lookupEnv "TORANGE_BOT_REDDIT_SECRET"

  username <- case usernameEnv of
                Nothing -> die "ERROR: No username provided."
                Just x -> pure x

  print $ doesSecretHave2FA secretEnv

  putStrLn $ "Hi, " <> username

doesSecretHave2FA :: Maybe String -> Bool
doesSecretHave2FA (Just env) = go env
  where
    go (x:xs) = case x of
                  ':' -> True
                  _ -> go xs
    go [] = False
doesSecretHave2FA Nothing = False


