{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

import Data.Map (Map)
import qualified Data.Map as Map

import AoC.D01
import AoC.D02
import AoC.D03
import AoC.D04
import AoC.D05
import AoC.D06
import AoC.D07
import AoC.D08
import AoC.D09

dayMap :: Map Int ([String] -> IO (), [String] -> IO ())
dayMap = Map.fromList [ (1, (d01p1, d01p2))
                      , (2, (d02p1, d02p2))
                      , (3, (d03p1, d03p2))
                      , (4, (d04p1, d04p2))
                      , (5, (d05p1, d05p2))
                      , (6, (d06p1, d06p2))
                      , (7, (d07p1, d07p2))
                      , (8, (d08p1, d08p2))
                      , (9, (d09p1, d09p2))
                      ]

data CmdFields = CmdFields {day :: Int, part :: Int}
  deriving (Show, Data, Typeable)

cmdField = CmdFields{day = 1 &= help "Selects the day of the challenge",
                    part = 1 &= help "Selects the part of the day"}

main :: IO ()
main = do
  args <- cmdArgs cmdField
  input <- getContents
  case Map.lookup (day args) dayMap of
    Just fun -> ((if (part args) == 1 then fst else snd) fun) $ lines input
    Nothing -> putStrLn "Please choose a day!"
