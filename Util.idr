module Advent.Util
import Data.Fuel
import Data.String
import System.File

||| A simple loop to perform IO for each line of input from stdin.
export
processStdin: (String -> IO ()) -> IO ()
processStdin process
   = when (not !(fEOF stdin))$ do
      Right line <- fGetLine stdin | Left _ => pure ()
      let trimmed = trim line
      when (not$ String.null trimmed)$ do
         process trimmed
         processStdin process

||| Selects the first element from a Stream that satisfies the predicate
export partial
first: ( a -> Bool ) -> Stream a -> a
first predicate (x :: xs) = if predicate x then x else first predicate xs

-- NOTE: redirection could be interesting with Chez scheme current-input/output-port
--export
--(<): IO () -> String -> IO ()
--procedure < inputFile = 
