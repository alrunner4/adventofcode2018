module Advent.Day3.Part1
import Advent.Day3
import Advent.Util
import Data.IORef
import Data.SortedMap

main: IO ()
main = do
   lines  <- newIORef$ the (List String) []
   claims <- newIORef$ SortedMap.empty {k=(Int,Int), v=Int}
   processStdin$ modifyIORef lines . (::)
   claims <- readIORef lines <&> Day3.parseClaims <&> foldl
      (\known,claim => merge @{Additive} known claim.positions)
      SortedMap.empty
   printLn$ foldl
      (\overlaps,claims_here => overlaps + if claims_here >= 2 then 1 else 0)
      0
      claims
