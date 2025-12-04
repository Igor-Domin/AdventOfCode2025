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

-- Much faster algo O(n) instead of 0(n2) 15s vs 1.4s: 1212 -> if 212121 contains 1212 -> 12 == 12
-- https://www.baeldung.com/cs/check-string-periodicity
-- isInvalidId :: Int -> Bool
-- isInvalidId i =
--   let s = show i
--       ss = s ++ s
--       mid = init (drop 1 ss)
--   in isInfixOf s mid

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