-- cparser.hs
-- A C-language parser lifted to product-lines
-- Ramy Shahin
-- Feb. 21st 2017
module CParser where
import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Syntax.AST
import SPL

parseC' :: Var InputStream -> Var Position -> Var (Either ParseError CTranslUnit)
parseC' = liftV2 parseC

inputStreamFromString' = liftV inputStreamFromString

startPosition :: String -> Position
startPosition fileName = position 0 fileName 0 0