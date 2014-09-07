import Control.Applicative

main :: IO ()
main = do
  let str = "123"
  print str
  where
    toInt :: String -> Int
    toInt = read
