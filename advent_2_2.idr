import Data.Vect


Identifier : ( length : Nat ) -> Type
Identifier { length } = Vect length Char


distance : Identifier length -> Identifier length -> Nat
distance l r = length( filter id ( zipWith(/=) ( toList l ) ( toList r ) ) )


commonality : { length : Nat } -> List( Identifier length ) -> List Char

commonality [] = []

commonality { length = Z } _ = []

commonality { length = S n } ( str :: strs ) = let
    here = head str
    continue = if all( == here )( map head strs )
        then ( here :: )
        else id
in continue( commonality( map tail ( str :: strs ) ) )


matchOffBy : Nat -> List( Identifier length ) -> List( List( Identifier length ) )
matchOffBy n ( str :: strs ) = let
    matchesStr = ( filter(\ s => distance str s == n ) strs )
in if isNil matchesStr
    then matchOffBy n strs
    else ( str :: matchesStr ) :: matchOffBy n strs


addIdentifierFromLine : ( n ** List( Vect n Char ) ) -> List Char
    -> ( n ** List( Vect n Char ) )

addIdentifierFromLine( Z ** ids ) line = case line of
    [] => ( Z ** ids )
    xs => ( length xs ** [ fromList xs ] )

addIdentifierFromLine( n ** ids ) line = case line of
    [] => ( n ** ids )
    xs => case decEq ( length xs ) n of
        No _ => ( n ** ids )
        Yes sameLength => ( length xs ** fromList xs :: rewrite sameLength in ids )


lineChars : String -> List Char
lineChars str = filter( not . isNL )( unpack str )


main : IO ()
main = processStdin( Z ** [] )
    (\ previous, line => ( "" , addIdentifierFromLine previous ( lineChars line ) ) )
    (\ ( _ ** ids ) => show $ map(\ matches => pack( commonality matches ) )
        ( matchOffBy 1 ids )
    )