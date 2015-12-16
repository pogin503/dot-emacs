import Control.Applicative
import Data.List
import Control.Monad

toInt :: String -> Int
toInt = read

main :: IO ()
main = do
  let str = "123"
  print str
