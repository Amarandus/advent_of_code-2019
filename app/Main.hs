{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

import Data.Map (Map)
import qualified Data.Map as Map

import AoC.D01
import AoC.D02

dayMap :: Map Int ([String] -> IO (), [String] -> IO ())
dayMap = Map.fromList [ (1, (d01p1, d01p2))
                      , (2, (d02p1, d02p2))
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
