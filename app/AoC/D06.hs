module AoC.D06(d06p1, d06p2) where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Data.List (splitAt, elemIndex, intersect)
import Data.Tree

import Debug.Trace (traceShowId)

type Identifier = String
data Orbit = Orbit Identifier Identifier
  deriving (Show, Eq)

findChildren :: [Orbit] -> Identifier -> (Identifier, [Identifier])
findChildren orbits name = (name, foldl addChild [] orbits)
  where
    addChild acc (Orbit main sat) = acc ++ (if name == main then [sat] else [])

parseOrbit :: String -> Maybe Orbit
parseOrbit s = do
  i <- elemIndex ')' s
  return $ Orbit (take i s) (drop (i + 1) s)

ancestors :: Identifier -> Tree Identifier -> Maybe [Identifier]
ancestors s (Node n c)
  | n == s = Just [n]
  | c == [] = Nothing
  | otherwise = do
      case catMaybes (map (ancestors s) c) of
        [] -> Nothing
        (xs:[]) -> Just (n : xs)
        
commonAncestor :: [Identifier] -> [Identifier] -> (Identifier, Int)
commonAncestor a b = (last commonPath, length commonPath)
  where
    commonPath = map (fst) $ takeWhile (\(a, b) -> a == b) $ zip a b

findPath :: [Identifier] -> [Identifier] -> [Identifier]
findPath a b = (reverse aPath) ++ [traceShowId cAncestor] ++ bPath
  where
    (cAncestor, sharedAncestors) = commonAncestor a b
    aPath = drop (sharedAncestors) a
    bPath = drop (sharedAncestors) b

findBetween :: Tree Identifier -> Identifier -> Identifier -> Maybe [Identifier]
findBetween t a b = do
  aAnc <- ancestors a t
  bAnc <- ancestors b t
  return $ findPath aAnc bAnc
  

d06p1 :: [String] -> IO ()
d06p1 lines = do
  print $ sum $ map (\(l, nodes) -> l * (length nodes)) $ zip [0..] $ levels orbitTree
  where
    parsedLines = catMaybes $ map parseOrbit lines
    orbitTree = unfoldTree (findChildren parsedLines) "COM"
  
d06p2 :: [String] -> IO ()
d06p2 lines = do
  case findBetween orbitTree "SAN" "YOU" of
    Just p -> do
      print $ p
      print $ (-3) + length p -- -2 for the two objects, -1 to count necessary orbit transfers
    Nothing ->
      putStrLn "Not able to find SAN or YOU"
  where
    parsedLines = catMaybes $ map parseOrbit lines
    orbitTree = unfoldTree (findChildren parsedLines) "COM"

    
