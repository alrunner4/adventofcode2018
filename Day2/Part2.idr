import Advent.Util
import Data.IORef
import Data.List
import Data.String
import Data.Vect
import Decidable.Equality

%default total

Identifier : ( width : Nat ) -> Type
Identifier { width } = Vect width Char


distance : Identifier width -> Identifier width -> Nat
distance l r = List.length( List.filter id ( zipWith(/=) ( toList l ) ( toList r ) ) )


Identifiers: Type
Identifiers = ( w ** List( Identifier w ) )


matchOffBy : Nat -> Identifiers -> List Identifiers
matchOffBy n ids@( _ ** str :: strs ) = let
    matchesStr = filter(\ s => distance str s == n ) strs
 in if null matchesStr
    then matchOffBy n (assert_smaller ids ( _ ** strs ))
    else ( _ ** str :: matchesStr ) :: matchOffBy n (assert_smaller ids ( _ ** strs ))
matchOffBy _ _ = []


addIdentifier : List Char -> Identifiers -> Identifiers
addIdentifier line ( Z ** _ ) = ( length line ** [fromList line] )
addIdentifier line ( n ** ids ) = if null line
    then ( n ** ids )
    else case decEq (length line) n of
        No _ => ( n ** ids )
        Yes sameLength => ( length line ** fromList line :: rewrite sameLength in ids )


commonality : Identifiers -> List Char
commonality ( _   ** [] ) = []
commonality ( Z   ** _  ) = []
commonality ( S n ** ss@( str :: strs ) ) = let
    here = head str
    keepCommonHeads = if all( == here )( map head strs )
        then ( here :: )
        else id
 in keepCommonHeads( commonality( _ ** map tail ss ) )


partial
main : IO ()
main = do
   boxIDs <- newIORef$ the Identifiers ( Z ** [] )
   Util.processStdin$ modifyIORef boxIDs . addIdentifier . unpack . trim
   readIORef boxIDs <&> matchOffBy 1 >>= traverse_( putStrLn . pack . commonality )
