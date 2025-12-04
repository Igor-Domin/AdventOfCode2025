import Data.List
import Data.Maybe
import System.IO
import Text.Read

data DialPosition = DialPosition {position :: Int, zeroHits :: Int} deriving (Show)

makeCombination :: String -> Maybe Int
makeCombination [] = Nothing
makeCombination (dir : rot) = do
  rotation <- readMaybe rot
  case dir of
    'L' -> Just (-rotation)
    'R' -> Just rotation
    _ -> Nothing

moveDial :: DialPosition -> Int -> DialPosition
moveDial (DialPosition pos zeros) rot
  | rot == 0 = DialPosition pos zeros
  | otherwise = moveDial (DialPosition newPosition newZeros) (rot - step)
  where
    step = if rot > 0 then 1 else -1
    newPosition = (pos + step) `mod` 100
    newZeros = if newPosition == 0 then zeros + 1 else zeros

inputCombinations :: DialPosition -> (DialPosition -> Int -> DialPosition) -> [Int] -> DialPosition
inputCombinations dial _ [] = dial
inputCombinations startDial dialRotationFunc (rotation : rotations) =
  let newDial = dialRotationFunc startDial rotation
   in inputCombinations newDial dialRotationFunc rotations

initDial = DialPosition 50 0

main :: IO ()
main = do
  safeCombinations <- lines <$> readFile "input.txt"
  let rotations = mapMaybe makeCombination safeCombinations
  let zeros = zeroHits $ inputCombinations initDial moveDial rotations
  print $ "Zero hits: " ++ show zeros