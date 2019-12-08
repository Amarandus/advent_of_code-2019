module AoC.D07(d07p1, d07p2) where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import Data.Vector hiding ((++), map, zip, dropWhile, reverse, take, tail)
import Control.Monad.State
import Data.List (permutations)

import Debug.Trace (traceShow, traceShowId)

import AoC.D05

buildMem :: String -> Vector Int
buildMem = fromList.catMaybes.(map (readMaybe)).(wordsWhen (== ','))

buildIntMachine :: String -> Input -> IntMachineState
buildIntMachine s i = (mem, Exec 0, i, [])
  where
    mem = buildMem s

runAmplifiers :: String -> [Int] -> Int
runAmplifiers controller phases = Prelude.foldl (evalAmplifier) 0 phases
  where
    parsedController = buildIntMachine controller
    evalAmplifier acc phase = Prelude.head $ evalState (runIntMachine) (parsedController [phase, acc])

runUntilOutput :: [Int] -> State IntMachineState [Int]
runUntilOutput input = do
  (mem, ps, i, o) <- get
  put (mem, ps, i ++ input, o)
  case ps of
    Exec _ ->
      case o of
        [] -> do
          state (\x -> ((), performOp x))
          runUntilOutput []
        _ -> do
          put (mem, ps, i ++ input, [])
          return $ o
    Halted -> do
      return $ o

isHalted :: IntMachineState -> Bool
isHalted (_, Halted, _, _) = True
isHalted (_, _, _, _) = False

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateUntil p f i
  | p i' = []
  | otherwise = [i'] ++ iterateUntil (p) (f) i' 
  where
    i' = f i

runAmplifierLoop' :: (Int, Vector IntMachineState) -> (Int, Vector IntMachineState)
runAmplifierLoop' (input, amps) = Prelude.foldl (execMachine) (input, amps) [0..(Data.Vector.length amps) - 1]
  where
    execMachine (input, amps) i = traceShow o $ (if (Prelude.length o) == 0 then 0 else Prelude.last o, amps // [(i, amp')])
      where
        (o, amp') = runState (runUntilOutput [input]) (amps ! i)

runAmplifierLoop :: String -> [Int] -> Int
runAmplifierLoop controller phases =
  fst $ Prelude.last $ iterateUntil (\(_, amps) -> isHalted $ Data.Vector.last amps) (runAmplifierLoop')  (0, amplifiers)
  where
    mem = buildMem controller
    amplifiers = fromList $ map (\x -> (mem, Exec 0, [x], [])) phases
    

d07p1 :: [String] -> IO ()
d07p1 lines = do
  print $ Prelude.maximum $ map (runAmplifiers controllerSoftware) inputs
  where
    controllerSoftware = lines !! 0
    inputs = permutations [0..4]
  
d07p2 :: [String] -> IO ()
d07p2 lines = do
  putStrLn $ show $ outputs
  putStrLn $ "Result: "
  putStrLn $ show $ Prelude.maximum $ outputs
  where
    controllerSoftware = lines !! 0
    inputs = permutations [5..9]
    outputs = map (runAmplifierLoop controllerSoftware) inputs
  
