import Control.Applicative
import qualified Data.Foldable as F

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

instance F.Foldable Tree where
	foldMap f Empty = mempty
	foldMap f (Node x l r) = F.foldMap f l `mappend`
					  f x `mappend`
					  F.foldMap f r

testTree = Node 5 (Node 3 (Node 1 Empty Empty) (Node 6 Empty Empty)) (Node 9 (Node 8 Empty Empty) (Node 10 Empty Empty) )  
