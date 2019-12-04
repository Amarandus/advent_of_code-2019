module AoC.D03(d03p1, d03p2) where

import Data.List (intersect, intersectBy, minimumBy)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

-- From: https://stackoverflow.com/a/4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case Prelude.dropWhile p s of
                   "" -> []
                   s' -> w : wordsWhen p s''
                     where (w, s'') = Prelude.break p s'

data Point = Point Int Int
  deriving (Show, Eq)

instance Ord Point where
  (<=) a b = (manhattanDistance a) <= (manhattanDistance b)

data Direction = U Int | D Int | L Int | R Int
  deriving (Show, Eq)

parseDirection :: String -> Maybe Direction
parseDirection ('U':xs) = U <$> readMaybe xs
parseDirection ('D':xs) = D <$> readMaybe xs
parseDirection ('L':xs) = L <$> readMaybe xs
parseDirection ('R':xs) = R <$> readMaybe xs
parseDirection _ = Nothing

traceDirection :: Direction -> [Point]
traceDirection (U x) = [Point   i    0  | i <- [0..x - 1]]
traceDirection (D x) = [Point (-i)   0  | i <- [0..x - 1]]
traceDirection (L x) = [Point   0  (-i) | i <- [0..x - 1]]
traceDirection (R x) = [Point   0    i  | i <- [0..x - 1]]

manhattanDistance :: Point -> Int
manhattanDistance (Point a b) = (abs $ a) + (abs $ b)

(+.) :: Point -> Point -> Point
(+.) (Point a b) (Point c d) = Point (a + c) (b + d)

(-.) :: Point -> Point -> Point
(-.) (Point a b) (Point c d) = Point (a - c) (b - d)


(+>) :: Point -> Direction -> Point
(+>) (Point a b) (U x) = Point (a + x) b
(+>) (Point a b) (D x) = Point (a - x) b
(+>) (Point a b) (L x) = Point a (b - x)
(+>) (Point a b) (R x) = Point a (b + x)

movePath :: [Direction] -> [Point]
movePath = tail . snd . (foldl (step) (Point 0 0, []))
  where
    step (p, path) v = (p +> v, path ++ (map (p +.) $ traceDirection v))


d03p1 :: [String] -> IO ()
d03p1 lines =
  putStrLn $ show (closestIntersection, manhattanDistance closestIntersection)
  where
    parsedLines = map (catMaybes . (map parseDirection) . wordsWhen (== ',')) lines
    wire1 = parsedLines !! 0
    wire2 = parsedLines !! 1
    intersections = (movePath wire1) `intersect` (movePath wire2)
    closestIntersection = minimum intersections

intersectSnd :: Eq b => [(a, b)] -> [(a, b)] -> [(a, a, b)]
intersectSnd xs ys = [(xa, ya, xb) | (xa, xb) <- xs, (ya, yb) <- ys, xb == yb]
  
d03p2 :: [String] -> IO ()
d03p2 lines = do
  putStrLn $ show intersections
  putStrLn $ show closestIntersection
  putStrLn $ show $ (\(a, b, _) -> a + b) closestIntersection
  where
    parsedLines = map (catMaybes . (map parseDirection) . wordsWhen (== ',')) lines
    wire1 = zip [1..] $ movePath $ parsedLines !! 0
    wire2 = zip [1..] $ movePath $ parsedLines !! 1
    intersections = intersectSnd wire1 wire2
    closestIntersection = minimumBy (\(a, b, _) (c, d, _)-> compare (a + b) (c + d)) intersections
