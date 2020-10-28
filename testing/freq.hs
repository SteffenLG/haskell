import Data.Map (Map)
import qualified Data.Map as Map

frequencies :: Ord a => [a] -> Map a Int
frequencies = foldl (\f c -> (Map.insertWith (+) c 1 f)) Map.empty

freq :: Ord a => [a] -> Map a Int
freq = Map.fromListWith (+) . flip zip [1,1..]