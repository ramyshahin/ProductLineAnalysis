module TestDFS where
{-
import Data.List

post :: [(Int,Int)] -> Int -> [Int]
post edges v = 
  if   empty edges
  then []
  else let h = head edges in
       if   fst h == v
       then fst h :: post (tail edges) v
       else          post (tail edges) v
-}
