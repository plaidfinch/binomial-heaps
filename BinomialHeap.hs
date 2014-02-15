{-# LANGUAGE GADTs, DataKinds, KindSignatures, StandaloneDeriving #-}

module BinomialHeap (empty, merge, insert, findMin, extractMin, deleteMin, fromList, toList) where

import Control.Applicative ((<$>))
import Data.List (unfoldr)
import Data.Maybe
import Control.Arrow (first,second)

data Nat = Z | S Nat deriving Show

infixr 5 :-:
infixr 5 :=:

data Straight (n :: Nat) a where
   End   :: Straight Z a
   (:-:) :: Tree (S n) a -> Straight n a -> Straight (S n) a
deriving instance Show a => Show (Straight n a)

data Tree (n :: Nat) a where
   Tree :: a -> Straight n a -> Tree (S n) a
deriving instance Show a => Show (Tree n a)

data Heap (n :: Nat) a where
   Empty :: Heap n a
   (:=:) :: Maybe (Tree n a) -> Heap (S n) a -> Heap n a
deriving instance Show a => Show (Heap n a)

empty :: Heap (S Z) a
empty = Empty

meld :: (Ord a) => Tree n a -> Tree n a -> Tree (S n) a
meld t@(Tree a ts) t'@(Tree b ts') =
   if a < b
      then Tree a (t' :-: ts)
      else Tree b (t  :-: ts')

merge :: (Ord a) => Heap n a -> Heap n a -> Heap n a
merge heap Empty = heap
merge Empty heap = heap
merge (Nothing :=: ts) (t' :=: ts') =
   t' :=: merge ts ts'
merge (t :=: ts) (Nothing :=: ts') =
   t  :=: merge ts ts'
merge (Just t :=: ts) (Just t' :=: ts') =
   Nothing :=: insertTree (meld t t') (merge ts ts')

insertTree :: Ord a => Tree n a -> Heap n a -> Heap n a
insertTree t = merge (Just t :=: Empty)

insert :: (Ord a) => a -> Heap (S Z) a -> Heap (S Z) a
insert a = insertTree (Tree a End)

root :: Tree n a -> a
root (Tree a _) = a

roots :: Heap n a -> [a]
roots Empty              = []
roots (Nothing :=: heap) = roots heap
roots (Just t  :=: heap) = root t : roots heap

findMin :: (Ord a) => Heap n a -> Maybe a
findMin heap = case roots heap of
   [] -> Nothing
   xs -> Just $ minimum xs

clean :: Heap n a -> Heap n a
clean Empty             = Empty
clean (Just t :=: heap) =
   Just t :=: clean heap
clean (Nothing :=: heap) =
   case clean heap of
      Empty -> Empty
      heap' -> Nothing :=: heap'

selectChildrenOf :: (Ord a) => a -> Heap n a -> Maybe (Heap (S Z) a, Heap n a)
selectChildrenOf _ Empty           = Nothing
selectChildrenOf val (mt :=: heap) =
   case mt of
      Just t | val == root t ->
         Just (heapFromChildren t, Nothing :=: clean heap)
      otherwise ->
         second (mt :=:) <$> selectChildrenOf val heap

heapFromChildren :: Tree n a -> Heap (S Z) a
heapFromChildren (Tree _ cs) = heapFromStraight cs

heapFromStraight :: Straight n a -> Heap (S Z) a
heapFromStraight straight = iter straight Empty
   where
      iter :: Straight n a -> Heap (S n) a -> Heap (S Z) a
      iter End        heap = heap
      iter (t :-: ts) heap = iter ts (Just t :=: heap)

extractMin :: (Ord a) => Heap (S Z) a -> Maybe (a, Heap (S Z) a)
extractMin heap = do
   heapMin           <- findMin heap
   (childHeap,heap') <- selectChildrenOf heapMin heap
   return (heapMin, merge childHeap heap')

deleteMin :: (Ord a) => Heap (S Z) a -> Heap (S Z) a
deleteMin h = maybe h snd (extractMin h)

fromList :: (Ord a) => [a] -> Heap (S Z) a
fromList = foldr insert empty

toList :: (Ord a) => Heap (S Z) a -> [a]
toList = unfoldr extractMin

heapSort :: (Ord a) => [a] -> [a]
heapSort = toList . fromList
