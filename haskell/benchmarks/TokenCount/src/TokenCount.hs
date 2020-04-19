module TokenCount where

import Lexer

tokenCount :: [CToken] -> Int
tokenCount xs = 
    case xs of 
        [] -> 0
        (y : ys) -> if fst y == TNil 
                    then tokenCount ys
                    else 1 + tokenCount ys