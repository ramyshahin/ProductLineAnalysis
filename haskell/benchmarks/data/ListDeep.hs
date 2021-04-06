module ListDeep where
import SPL
import VPreludeDeep
import Debug.Trace

data List_ a = List_ { f_Nil :: Maybe (I_Nil a), f_Cns :: Maybe (I_Cns a) }
  deriving (Show)

d_List_ = List_ Nothing Nothing --d_Nil d_Cns
data I_Nil a = I_Nil
  deriving (Show)

data I_Cns a = I_Cns (Var a) (List_ a)
  deriving (Show)

d_Nil = I_Nil
d_Cns = I_Cns d_Var d_List_
c_Nil = List_ (Just I_Nil) Nothing -- d_Cns
c_Cns v0 v1 = List_ Nothing (Just (I_Cns v0 v1))
len :: List_ a -> Int_
len l  = unions [
    (\x -> case x of 
             Nothing -> Var []
             (Just I_Nil) -> (0 ^| ttPC)) (f_Nil (l)), 
    (\x -> case x of
             Nothing -> Var []
             (Just (I_Cns x xs)) -> ((+) ^| ttPC) <*> ((1 ^| ttPC)) <*> (len xs)) (f_Cns (l))]

