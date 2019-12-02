module AoC.D01(d01p1, d01p2) where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Data.Function (fix)

-- Fuel required to launch a given module is based on its mass.
-- Specifically, to find the fuel required for a module, take its mass,
-- divide by three, round down, and subtract 2.
calcFuel :: Int -> Int
calcFuel mass
  | fuel >= 0 = fuel
  | otherwise = 0
  where
    fuel = (floor $ (fromIntegral mass) / 3) - 2

calcTotalFuel :: (Int -> Int) -> [Int] -> Int
calcTotalFuel f = foldl (\acc v -> acc + f v) 0

calcCorrectFuel :: Int -> Int
calcCorrectFuel = sum . (takeWhile (> 0)) . (iterate calcFuel) . calcFuel

d01p1 :: [String] -> IO ()
d01p1 lines =
  putStrLn $ "The total fuel required is " ++ show totalFuel
  where
    parsedLines = catMaybes $ map readMaybe lines
    totalFuel = calcTotalFuel (calcFuel) parsedLines
  
d01p2 :: [String] -> IO ()
d01p2 lines = 
  putStrLn $ "The total fuel required is " ++ show totalFuel
  where
    parsedLines = catMaybes $ map readMaybe lines
    totalFuel = calcTotalFuel (calcCorrectFuel) parsedLines
