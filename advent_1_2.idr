import Control.Monad.State
import Data.SortedSet -- compile this import with `-p contrib`


evaluate : List Int -> String
evaluate [] = "no input"
evaluate( x :: xs ) = show( evalState( feed loop )( 0 , empty ) ) where

	loop : Stream Int
	loop = Stream.cycle( x :: xs )

	step : Int -> State( Int , SortedSet Int )( Maybe Int )
	step modifier = do
		( last_frequency , frequencies ) <- get
		let new_frequency = last_frequency + modifier
		if contains new_frequency frequencies
			then pure( Just new_frequency )
			else do
				put( new_frequency , insert new_frequency frequencies )
				pure Nothing

	feed : Stream Int -> State( Int , SortedSet Int ) Int
	feed stream = do
		Just matched_frequency <- step( head stream )
			| Nothing => feed ( tail stream )
		pure matched_frequency


main : IO ()
main = processStdin []
	(\ previous_lines, line => ( "" , cast line :: previous_lines ) )
	(\ all_lines => evaluate( reverse all_lines ) )