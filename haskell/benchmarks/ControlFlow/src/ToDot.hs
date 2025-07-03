module ToDot where

import CFG
import System.IO

toDot :: String -> CFG -> IO ()
toDot filename (CFG nodes edges) = do
    handle <- openFile filename WriteMode
    hPutStr handle "digraph {"

    hPutStr handle "}"
    hClose handle