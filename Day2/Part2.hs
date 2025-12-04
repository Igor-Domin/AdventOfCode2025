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

splitEachIndex :: String -> Int -> [String]
splitEachIndex s i
  | s == "" || i >= len || i <= 0 = [s]
  | otherwise =
      let (chunk, rest) = splitAt i s
       in case rest of
            [] -> [chunk]
            _ -> chunk : splitEachIndex rest i
  where
    len = length s

isInvalidId :: Int -> Bool
isInvalidId id = go $ div (length s) 2
  where
    s = show id
    go i
      | i <= 0 = False
      | otherwise =
          let (x : xs) = splitEachIndex s i
           in all (== x) xs || go (i - 1)

main :: IO ()
main = do
  ids <- readFile "input.txt"
  print $ sum $ concatMap (filter isInvalidId . rangeFromString) (splitOnChar ',' ids)