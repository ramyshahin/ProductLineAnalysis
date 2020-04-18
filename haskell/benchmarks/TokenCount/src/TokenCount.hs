module TokenCount where

import Lexer

tokenCount :: [CToken] -> Int
tokenCount xs = 
    if null xs then
        0
    else if fst (head xs) == TNil then 
        tokenCount (tail xs)
    else
        1 + tokenCount (tail xs)