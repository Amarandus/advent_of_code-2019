{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

import Data.Map (Map)
import qualified Data.Map as Map

import AoC.D01

dayMap :: Map Int (IO ())
dayMap = Map.fromList [(1, d01main)]

data CmdFields = CmdFields {day :: Int}
  deriving (Show, Data, Typeable)

cmdField = CmdFields{day = 1 &= help "Selects the day of the challenge"}

main :: IO ()
main = do
  args <- cmdArgs cmdField
  case Map.lookup (day args) dayMap of
    Just x -> x
    Nothing -> putStrLn "Please choose a day!"
