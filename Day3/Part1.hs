import Data.Char
import Data.List
import Data.Maybe
import System.IO

quickJolt :: [Int] -> Int
quickJolt bank = 10 * maxJolt + maximum (drop (maxIndex + 1) bank)
  where
    maxJolt = maximum (init bank)
    maxIndex = fromJust (elemIndex maxJolt bank)

main :: IO ()
main = do
  banks <- lines <$> readFile "input.txt"
  print $ sum $ map (quickJolt . map digitToInt) banks
