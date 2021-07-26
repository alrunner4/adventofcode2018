module Advent.Day2.Part1
import Advent.Util
import Data.IORef
import Data.SortedMap


countLetters : String -> SortedMap Char Int
countLetters = foldl accumulate empty . unpack where
   accumulate : SortedMap Char Int -> Char -> SortedMap Char Int
   accumulate counts char = case lookup char counts of
      Nothing => insert char 1 counts
      Just n  => insert char ( n + 1 ) counts


hasMultiples : Int -> SortedMap a Int -> Bool
hasMultiples n = any( n == ) . values


main : IO ()
main = do
   twos <- newIORef 0
   threes <- newIORef 0
   processStdin \line => do
      let counts = countLetters line
      when (hasMultiples 2 counts) (modifyIORef twos (+1))
      when (hasMultiples 3 counts) (modifyIORef threes (+1))
   printLn$ !(readIORef twos) * !(readIORef threes)
