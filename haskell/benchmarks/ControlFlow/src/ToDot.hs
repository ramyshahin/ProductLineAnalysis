module ToDot where

import CFG
import ListDeep
import SPL
import qualified Data.List as L
import System.IO

nodeID :: t -> String
nodeID x = "n" ++ show (addr x)

clusterID :: t -> String
clusterID x = "cluster" ++ show (addr x)

vToString :: Show t => V t -> String
vToString (V xs) =
    foldr (\(x,pc) s -> s ++ show x) "" xs

node :: Handle -> String -> String -> IO ()
node h id label = do
    let isProxy = label == ""
    hPutStr h $ id ++ "[label=\"" ++ label ++ "\""
    if isProxy then
        hPutStrLn h ";style=filled;fillcolor=blue]"
    else
        hPutStrLn h "]"

edge :: Handle -> String -> String -> Maybe String -> PresenceCondition -> IO ()
edge h x y c pc = do 
    hPutStr h $ x ++ " -> " ++ y ++ " [label = \"" ++ (show pc) ++ "\""
    case c of
        Nothing -> return ()
        Just c' -> hPutStr h $ "; lhead = " ++ c'
    hPutStrLn h "]"

toDotCFG :: String -> CFG -> IO ()
toDotCFG filename (CFG nodes edges) = do
    handle <- openFile filename WriteMode
    hPutStr handle "digraph {\n"
    hPutStr handle "}\n"
    hClose handle

-------------------
-- VList
-------------------
toDotVList :: Handle -> Maybe (Val (I_List String)) -> VList String -> IO ()
toDotVList h parent v@(V xs) = do
    hPutStr h $ "subgraph " ++ (clusterID v) ++ " { rank = same; "
    mapM_ (\(x,_) -> hPutStr h $ (nodeID x) ++ "; ") xs
    hPutStrLn h "}"
    mapM_ (\(x,pc) -> toDotIList h parent v x pc) xs

intersect :: PresenceCondition -> PresenceCondition -> Bool
intersect pc1 pc2 = pc1 /\ pc2 /= noConfigs

proxyEdge :: Handle -> (Val (I_List t)) -> VList t -> IO ()
proxyEdge h (x,pc) v@(V xs) = do
    mapM_ (\(x', pc') -> 
                if intersect pc pc'
                then edge h (nodeID x) (nodeID x') (Just (clusterID v)) pc
                else return ()
          ) xs 

toDotIList :: Handle -> Maybe (Val (I_List String)) -> VList String -> I_List String -> PresenceCondition -> IO ()
toDotIList h parent v l pc =
    case l of
        Proxy_List xs -> do
            node h (nodeID l) ""
            case parent of
                Nothing -> return ()
                Just (p, pc') -> 
                    if intersect pc pc' then 
                        edge h (nodeID p) (nodeID l) (Just (clusterID v)) pc
                    else return ()
            proxyEdge h (l, pc) xs
        I_Nil -> return ()
        I_Cons v@(V ys) xs -> do
            let lbl = L.intercalate ", " $ map fst ys
            node h (nodeID l) lbl
            case parent of
                Nothing -> return ()
                Just (p, pc') -> 
                    if intersect pc pc' then 
                        edge h (nodeID p) (nodeID l) (Just (clusterID v)) pc  
                    else return ()      
            toDotVList h (Just (l, pc)) xs

toDotDeepList :: String -> VList String -> IO ()
toDotDeepList filename xs = do
    handle <- openFile filename WriteMode
    hPutStrLn handle "digraph {"
    hPutStrLn handle "compound=true;"
    hPutStrLn handle "node[shape=box]"
    toDotVList handle Nothing xs
    hPutStrLn handle "}"
    hClose handle