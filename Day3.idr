module Advent.Day3
import Data.Either
import Data.List
import Data.SortedMap
import Text.Lexer
import Text.Parser

public export
record Claim where
   constructor ElfClaim
   ident:  Int
   left:   Int
   top:    Int
   width:  Int
   height: Int

public export
Show Claim where
   show c = 
      "#" ++ show (ident c) ++
      " @ " ++ show (left c) ++ "," ++ show (top c) ++
      ": " ++ show (width c) ++ "x" ++ show (height c)

export
(.positions): Claim -> List(Int,Int)
(.positions) claim = [ (x,y) |
   x <- [ claim.left .. claim.left + claim.width  - 1 ],
   y <- [ claim.top  .. claim.top  + claim.height - 1 ]]

namespace SortedMap
   export
   (.positions): Claim -> SortedMap(Int,Int) Int
   (.positions) = fromList . map (,1) . (.positions)

||| Note: punctuation are indistinguishable from one another in this simple token representation.
data ClaimTokens = I | Punct
TokenKind ClaimTokens where
   TokType I     = Int
   TokType Punct = String
   tokValue = \case
      I     => cast
      Punct => id

ClaimToken: Type
ClaimToken = Token ClaimTokens

lexFully: TokenMap t -> String -> Maybe (List t)
lexFully tokenMap str = case lex tokenMap str of
   (tokens, (_, (_, ""))) => Just (map tok tokens)
   _ => Nothing

lexClaims: List String -> List (List ClaimToken)
lexClaims strs = let
   claimTokenMap = toTokenMap [
      (spaces, Punct),
      (is '#', Punct),
      (is '@', Punct),
      (is ',', Punct),
      (is ':', Punct),
      (is 'x', Punct),
      (intLit, I)]
 in mapMaybe (lexFully claimTokenMap) strs

claimGrammar: Grammar ClaimToken True Claim
claimGrammar = let
   punct = \label => terminal label (\t => case kind t of { Punct => Just (); _ => Nothing })
   num   = \label => terminal label (\t => case kind t of { I => Just (tokValue I (text t)); _ => Nothing })
 in ElfClaim
      <$> (punct "#" *> num "claim ID")
      <*> (punct "space" *> punct "@" *> punct "space" *> num "left-inches")
      <*> (punct "," *> num "right-inches")
      <*> (punct ":" *> punct "space" *> num "width-inches")
      <*> (punct "x" *> num "height-inches")

export
parseClaims: List String -> List Claim
parseClaims strs = mapMaybe
   (\tokens => map fst (eitherToMaybe (parse claimGrammar tokens)))
   (lexClaims strs)

