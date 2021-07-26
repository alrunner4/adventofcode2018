module Advent.Day1
import Advent.Util
import Data.IORef
import Data.List
import Data.SortedSet
import Data.Stream


export
Part1 : IO ()
Part1 = do
   calibration <- newIORef 0
   Util.processStdin \input_line => modifyIORef calibration (+ (cast input_line))
   printLn !(readIORef calibration)


export
Part2: IO ()
Part2 = do
   changeListLIFO <- newIORef []
   Util.processStdin \input_line => modifyIORef changeListLIFO (cast input_line ::)
   putStrLn$ frequencyScanner$ reverse$ !(readIORef changeListLIFO)
   where

      frequencyScanner : List Int -> String
      frequencyScanner [] = "error: no input"
      frequencyScanner changeList@(_ :: _) = show firstRepeat
      where

         startingFrequency : Int
         startingFrequency = 0

         accumulatedWithHistory : Stream (Int, SortedSet Int)
         accumulatedWithHistory = scanl
            (\(current,reached), change => (current+change, insert current reached))
            (startingFrequency, SortedSet.empty)
            (Stream.cycle changeList)

         firstRepeat : Int
         firstRepeat = fst$ Util.first (uncurry SortedSet.contains) accumulatedWithHistory
