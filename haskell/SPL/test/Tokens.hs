module Tokens where
{-
import SPL
import PropBDD
import Control.Applicative

u :: Universe
u@[a, b] = mkUniverse ["A", "B"]

notA = neg a
notB = neg b 

a_b = conj[a, b]
a_Notb = conj[a, notB]
notA_b = conj[notA, b]
notA_notB = conj[notA, notB]

type Token = String

-- TokenSeq data structure
data TokenSeq =
   NilSeq
 | TokenSeq Token TokenSeq
 deriving(Eq)

mkTokenSeq :: [String] -> TokenSeq
mkTokenSeq [] = NilSeq
mkTokenSeq (x : xs) = TokenSeq x (mkTokenSeq xs)

seqTail :: TokenSeq -> TokenSeq
seqTail NilSeq = NilSeq
seqTail (TokenSeq _ t) = t 

lengthTokenSeq :: TokenSeq -> Int
lengthTokenSeq s =
    if (s == NilSeq) then 0 else 1 + (lengthTokenSeq (seqTail s))

concatTokenSeq :: TokenSeq -> TokenSeq -> TokenSeq
concatTokenSeq NilSeq ys = ys
concatTokenSeq (TokenSeq x xs) ys = TokenSeq x (concatTokenSeq xs ys)

-- Lifted TokenSeq (shallow)
type TokenSeq' = Var TokenSeq

_NilSeq' = mkVarT NilSeq

mkTokenSeq' :: Var [String] -> Var TokenSeq
mkTokenSeq' = liftA mkTokenSeq

seqTail' = liftA seqTail

lengthTokenSeq' :: TokenSeq' -> Var Int
lengthTokenSeq' = liftA lengthTokenSeq

concatTokenSeq' :: TokenSeq' -> TokenSeq' -> TokenSeq' 
concatTokenSeq' = liftA2 concatTokenSeq

foldConcat :: [TokenSeq'] -> TokenSeq' 
foldConcat = foldr concatTokenSeq' _NilSeq' 

-- deep lifting
eq' = liftA2 (==)
plus' = liftA2 (+)

lengthTokenSeq'' :: Var TokenSeq -> Var Int
lengthTokenSeq'' s = 
    cond' (eq' s _NilSeq') 
          (mkVarT 0)
          (plus' (mkVarT 1) (lengthTokenSeq'' (seqTail' s)))

-- single product data
singleProd = "int foo ( int a , int b , int c ) { return ( a  + b ) * c ; }"

singleProdTokens = words singleProd

singleProdTokenSeq = mkTokenSeq singleProdTokens

-- product line
_mkVar xs pc = Var[(xs, pc), ([], (neg pc))] 
seg_ = _mkVar (words "int foo") tt
seg0 = _mkVar (words "( int a ) {") a
seg1 = _mkVar (words "return a * 2 ;") a_b
seg2 = _mkVar (words "return a * 3 + 1 ;") a_Notb
seg3 = _mkVar (words "}") a
seg4 = _mkVar (words "( int a , int b ) { return ( a ") notA
seg5 = _mkVar (words "+") notA_b
seg6 = _mkVar (words "* 2 +") notA_notB
seg7 = _mkVar (words "b ) ; }") notA

fullSeq :: TokenSeq' 
fullSeq = foldConcat (map mkTokenSeq' [seg_, seg0, seg1, seg2, seg3, seg4, seg5, seg6, seg7])
-}