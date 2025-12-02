import System.IO
import Data.List
import Text.Read
import Data.Maybe

makeCombination :: String -> Maybe Int
makeCombination [] = Nothing
makeCombination (dir:rot) = do
    rotation <- readMaybe rot
    case dir of
      'L' -> Just (-rotation)
      'R' -> Just rotation
      _ -> Nothing

moveDial :: Int -> Int -> Int
moveDial dial rotation
  | rotation < 0 && dial + rotation >= 0 = dial + rotation
  | rotation < 0 && dial + rotation < 0 = moveDial 100 (rotation + dial)
  | rotation > 0 && dial + rotation <= 99 = dial + rotation
  | rotation > 0 && dial + rotation > 99 = moveDial 0 (rotation+(dial-100))
  | rotation == 0 = dial

countSecretZeros :: Int -> (Int -> Int -> Int) -> [Int] -> Int
countSecretZeros _ _ [] = 0
countSecretZeros startDial dialRotationFunc (rotation:rotations) =
  let dial = dialRotationFunc startDial rotation
      zeroHit = if dial == 0 then 1 else 0
  in zeroHit + countSecretZeros dial dialRotationFunc rotations

initDial = 50

main :: IO ()
main = do
  safeCombinations <- lines <$> readFile "input.txt"
  let rotations = mapMaybe makeCombination safeCombinations
  let zeros = countSecretZeros initDial moveDial rotations 
  print zeros