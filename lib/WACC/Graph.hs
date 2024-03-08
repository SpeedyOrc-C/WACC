module WACC.Graph where

import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Arrow

newtype Graph a = Graph (M.Map a (S.Set a)) deriving Show

instance Eq a => Eq (Graph a) where
    (==) :: Eq a => Graph a -> Graph a -> Bool
    Graph a == Graph b = a == b

fromListWith :: Ord a => (S.Set a -> S.Set a -> S.Set a) -> [(a, S.Set a)] -> Graph a
fromListWith f = Graph . M.fromListWith f

fromList :: Ord a => [(a, S.Set a)] -> Graph a
fromList = Graph . M.fromList

fromList' :: Ord a => [(a, [a])] -> Graph a
fromList' = Graph . M.fromList . map (second S.fromList)

closure :: Ord a => Graph a -> Graph a -> Graph a
closure (Graph a) (Graph b) = fromList' $ do
    node1 <- M.keys a
    case do
        node2 <- S.toList (a M.! node1)
        maybe [] S.toList (b M.!? node2) of
        [] -> []
        node3s -> [(node1, node3s)]

union :: Ord a => Graph a -> Graph a -> Graph a
union (Graph a) (Graph b) = Graph $ M.unionWith S.union a b

unions :: Ord a => [Graph a] -> Graph a
unions = foldl union empty

transSelf :: Ord a => Graph a -> Graph a
transSelf g = g `union` closure g g

transitiveClosure :: Ord a => Graph a -> Graph a
transitiveClosure g = if g == g' then g else transitiveClosure g'
    where g' = transSelf g

singleton :: a -> S.Set a -> Graph a
singleton from destinations = Graph $ M.singleton from destinations

empty :: Graph a
empty = Graph M.empty

headEdges :: Graph k -> [k]
headEdges (Graph g) = M.keys g

headEdgesSet :: Graph k -> S.Set k
headEdgesSet (Graph g) = M.keysSet g

(!) :: Ord k => Graph k -> k -> S.Set k
Graph g ! i = g M.! i
