import Reddit.Actions.Post
import System.Environment

main :: IO ()
main = do
  username <- getEnv "TORANGE_BOT_REDDIT_USERNAME"
  secret <- getEnv "TORANGE_BOT_REDDIT_SECRET"
  putStrLn $ "Hi, " <> username


