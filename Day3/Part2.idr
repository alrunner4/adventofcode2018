module Advent.Day3.Part2
import Advent.Day3
import Advent.Util
import Data.IORef
import Data.SortedMap
import Data.SortedSet

data SwatchState = ClaimedBy Int | Conflicted

main: IO ()
main = do
   lines <- newIORef []
   processStdin$ modifyIORef lines . (::)
   fabric <- newIORef( SortedMap.empty {k=(Int,Int), v=SwatchState} )
   candidates <- newIORef SortedSet.empty

   for_( parseClaims !(readIORef lines) ) \claim => do
      modifyIORef candidates (insert claim.ident)
      for_ claim.positions \p => case lookup p !(readIORef fabric) of
         Just (ClaimedBy prior) => do
                                   modifyIORef candidates (delete prior . delete claim.ident)
                                   modifyIORef fabric (insert p Conflicted)
         Just Conflicted        => modifyIORef candidates (delete claim.ident)
         Nothing                => modifyIORef fabric (insert p (ClaimedBy claim.ident))

   printLn !(readIORef candidates)
