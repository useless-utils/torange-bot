import Reddit.Actions.Post
import System.Environment

main :: IO ()
main = do
  username <- getEnv "TORANGE_BOT_REDDIT_USERNAME"
  secret <- lookupEnv "TORANGE_BOT_REDDIT_SECRET"
  print $ doesSecretHave2FA secret
  putStrLn $ "Hi, " <> username

doesSecretHave2FA :: Maybe String -> Bool
doesSecretHave2FA (Just env) = go env
  where
    go (x:xs) = case x of
                  ':' -> True
                  _ -> go xs
    go [] = False
doesSecretHave2FA Nothing = False
