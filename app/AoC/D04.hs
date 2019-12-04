module AoC.D04(d04p1, d04p2) where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

import Data.Foldable (for_)
import Data.SBV

val :: [SInteger] -> SInteger
val = foldr1 (\d r -> d + 10*r) . reverse


puzzle1 :: (Integer, Integer) -> Symbolic ()
puzzle1 (r1, r2) = do
  ds@[d1, d2, d3, d4, d5, d6] <- sIntegers ["d1", "d2", "d3", "d4", "d5", "d6"]
  for_ ds $ \d -> constrain $ inRange d (0,9)
  constrain $ inRange (val ds) (literal r1, literal r2)
  constrain $ sAll (\(x, y) -> x .<= y) $ zip ds (tail ds)
  constrain $ sAny (\(x, y) -> x .== y) $ zip ds (tail ds)

slice :: Int -> Int -> [a] -> [a]
slice a b xs = take (b - a) $ drop a xs

findTFT :: [SBool] -> SBool
findTFT [] = sFalse
findTFT (_:[]) = sFalse
findTFT (_:_:[]) = sFalse
findTFT x@(_:xs) = sAny (id) [isEq (slice 0 3 x) pat, findTFT xs]
  where
    isEq a b = sAll (\(x, y) -> x .== y) $ zip a b
    pat = [sFalse, sTrue, sFalse]

puzzle2 :: (Integer, Integer) -> Symbolic ()
puzzle2 (r1, r2) = do
  ds@[d1, d2, d3, d4, d5, d6] <- sIntegers ["d1", "d2", "d3", "d4", "d5", "d6"]
  for_ ds $ \d -> constrain $ inRange d (0,9)
  constrain $ inRange (val ds) (literal r1, literal r2)
  constrain $ sAll (\(x, y) -> x .<= y) $ zip ds (tail ds)
  constrain $ sAny (id) $ pairs ds
  constrain $ sTrue .== findTFT ([sFalse] ++ (pairs ds) ++ [sFalse])
  where
    pairs v = map (\(x, y) -> x .== y) $ zip v (tail v)

d04p1 :: [String] -> IO ()
d04p1 lines = do
  solutions <- allSat $ puzzle1 input
  putStrLn $ show solutions
  where
    input = (read (lines !! 0), read (lines !! 1))
    
d04p2 :: [String] -> IO ()
d04p2 lines = do
  solutions <- allSat $ puzzle2 input
  putStrLn $ show solutions
  where
    input = (read (lines !! 0), read (lines !! 1))
