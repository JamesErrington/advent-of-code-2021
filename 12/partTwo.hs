import Data.Char (ord)
import qualified Data.List as L (nub)
import qualified Data.Map as M (Map, fromListWith, lookup, toList)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

type Node = String

type Graph = M.Map Node [Node]

type Path = [Node]

parseEdge :: String -> [(Node, [Node])]
parseEdge s = fmap pure <$> [e, swap e]
  where
    e = tail <$> span (/= '-') s

toGraph :: [String] -> Graph
toGraph = M.fromListWith (++) . concatMap parseEdge

isStart :: Node -> Bool
isStart = (==) "start"

isEnd :: Node -> Bool
isEnd = (==) "end"

isSmall :: Node -> Bool
isSmall = all ((> 90) . ord)

isBig :: Node -> Bool
isBig = all ((< 97) . ord)

frequency :: Path -> [(Node, Int)]
frequency xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

validNode :: Path -> Node -> Bool
validNode path node
  | isStart node = False
  | isBig node || isEnd node = True
  | isDuplicate = node `notElem` path
  | otherwise = True
  where
    isDuplicate = any ((> 1) . snd) $ frequency $ filter isSmall path

paths :: Graph -> [Path]
paths graph = paths' graph [] "start"

paths' :: Graph -> Path -> Node -> [Path]
paths' graph path node
  | isEnd node = [newpath]
  | otherwise = concatMap (paths' graph newpath) ns
  where
    newpath = node : path
    ns = filter (validNode newpath) $ fromJust $ M.lookup node graph

main :: IO ()
main = do
  graph <- toGraph . lines <$> readFile "12/data.txt"
  print $ length $ paths graph
