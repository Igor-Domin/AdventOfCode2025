import Data.List
import System.IO

trim :: String -> String
trim = dropWhileEnd (== ' ') . dropWhile (== ' ')

splitOnChar :: Char -> String -> [String]
splitOnChar _ "" = []
splitOnChar c s =
  let (x, xs) = break (== c) s
   in case xs of
        [] -> [x]
        (_ : xs) -> trim x : splitOnChar c xs

rangeFromString :: String -> [Int]
rangeFromString s =
  let [aStr, bStr] = splitOnChar '-' s
      a = read aStr
      b = read bStr
   in [a .. b]

isInvalidId :: Int -> Bool
isInvalidId i =
  let s = show i
      len = length s
      (a, b) = splitAt (len `div` 2) s
   in even len && (a == b)

isInvalidArr :: [Int] -> Bool
isInvalidArr i = True

main :: IO ()
main = do
  ids <- readFile "input.txt"
  print $ sum $ concatMap (filter isInvalidId . rangeFromString) (splitOnChar ',' ids)