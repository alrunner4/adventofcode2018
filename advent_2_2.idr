import Data.SortedMap
import Data.Vect


countLetters : String -> SortedMap Char Int
countLetters string = let letters = unpack string in foldl accumulate empty letters where

	accumulate : SortedMap Char Int -> Char -> SortedMap Char Int
	accumulate counts char = case lookup char counts of
		Nothing => insert char 1 counts
		Just n  => insert char ( n + 1 ) counts


Identifier : Type
Identifier = Vect 26 Int


alphabet : Vect 26 Char
alphabet = fromList( enumFromTo 'a' 'z' )


makeIdentifier : SortedMap Char Int -> Identifier
makeIdentifier counts = map(\ letter => maybe 0 id ( lookup letter counts ) ) alphabet


{- Vector Utilities -}


dot : Num i => Vect dim i -> Vect dim i -> i
dot l r = sum( map(\(el, er) => el*er )( Vect.zip l r ) )


magnitude : Foldable t => Functor t => t Double -> Double
magnitude v = sqrt( sum( map(\x => x*x ) v ) )


vcos : Vect dim Double -> Vect dim Double -> Double
vcos l r = dot l r / ( magnitude l * magnitude r )