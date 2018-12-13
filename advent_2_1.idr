import Data.SortedMap


countLetters : String -> SortedMap Char Int
countLetters string = let letters = unpack string in foldl accumulate empty letters where

	accumulate : SortedMap Char Int -> Char -> SortedMap Char Int
	accumulate counts char = case lookup char counts of
		Nothing => insert char 1 counts
		Just n  => insert char ( n + 1 ) counts


hasMultiples : Int -> SortedMap a Int -> Bool
hasMultiples n = any( n == ) . values


main : IO ()
main = processStdin (0,0) (\
	( twos , threes ), line => let
		counts    = countLetters line
		addTwos   = if hasMultiples 2 counts then 1 else 0
		addThrees = if hasMultiples 3 counts then 1 else 0
	in ( "" , ( twos + addTwos , threes + addThrees ) )
) (\ ( totalTwos, totalThrees ) => show( totalTwos * totalThrees ) )