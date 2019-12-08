module AoC.D08(d08p1, d08p2) where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Data.List (minimumBy, concat)

data Color = Black | White | Transparent
  deriving (Eq, Show, Ord)

joinColor :: Color -> Color -> Color
joinColor Black _ = Black
joinColor White _ = White
joinColor Transparent x = x

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs
  | n <= 0 = []
  | n > length xs = [xs]
  | n > length nextPart = [xs]
  | otherwise = [take n xs] ++ (splitEvery n $ drop n xs)
  where
    nextPart = drop n xs

mkColor :: Char -> Color
mkColor '0' = Black
mkColor '1' = White
mkColor _ = Transparent

mkPixel :: Color -> Char
mkPixel Black = '█'
mkPixel _ = ' '

splitToLayers :: (Int, Int) -> [Char] -> [[[Color]]]
splitToLayers (w, h) xs = map (splitEvery w) $ splitEvery (w * h) $ map mkColor xs

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences c xs = length $ filter (== c) xs


d08p1 :: [String] -> IO ()
d08p1 lines = do
  mapM_ (putStrLn . show) $ zip [0..] image
  putStrLn . show $ minimalLayer
  putStrLn . show $ (countOccurrences White $ concat minimalLayer) * (countOccurrences Transparent $ concat minimalLayer)
  where
    imageSize = (25, 6)
    image = splitToLayers imageSize $ lines !! 0
    minimalLayer = minimumBy (\a b -> (countOccurrences Black $ concat a) `compare` (countOccurrences Black $ concat b)) image
  
d08p2 :: [String] -> IO ()
d08p2 lines =
  mapM_ (putStrLn . map (mkPixel)) image
  where
    imageSize = (25, 6)
    imageLayers = splitToLayers imageSize $ lines !! 0
    image = foldl (\acc v -> map (\(x, y) -> map (uncurry joinColor) $ zip x y) $ zip acc v) (imageLayers !! 0) (drop 1 imageLayers)
