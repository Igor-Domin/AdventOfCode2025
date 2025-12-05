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

{-

  Much faster algo O(n) instead of 0(n2): 1212 -> if 212121 contains 1212 -> 12 == 12
  https://www.baeldung.com/cs/check-string-periodicity
  12.99s vs 1.4s in GHC interpreted mode, when it's compiled it's 0.29s vs 0.5s
  It's because O(n2) is worst case scenario which literally never happen here, and O(n) in this algorithm, does go thru all n
  It would be different if id were much longer

isInvalidId :: Int -> Bool
isInvalidId i =
  let s = show i
      ss = s ++ s
      mid = init (drop 1 ss)
   in isInfixOf s mid

-}

isInvalidId :: Int -> Bool
isInvalidId id = go $ div len 2
  where
    len = length s
    s = show id
    go i
      | i <= 0 = False
      | mod len i /= 0 = go (i - 1)
      | otherwise =
          let (x : xs) = splitEachIndex s i
           in all (== x) xs || go (i - 1)

main :: IO ()
main = do
  ids <- readFile "input.txt"
  print $ sum $ concatMap (filter isInvalidId . rangeFromString) (splitOnChar ',' ids)