{-# LANGUAGE GADTs, DataKinds, KindSignatures, StandaloneDeriving #-}

module BinomialHeap
   (empty, merge, insert, findMin, extractMin, deleteMin, fromList, toList)
   where

import Control.Applicative ((<$>))
import Control.Arrow
import Data.List (unfoldr)

data Nat = Z | S Nat deriving Show

infixr 5 :-:
infixr 5 :=:

data Straight (n :: Nat) a where
   E :: Straight Z a
   (:-:) :: Tree (S n) a -> Straight n a -> Straight (S n) a
deriving instance Show a => Show (Straight n a)

data Tree (n :: Nat) a where
   N :: a -> Straight n a -> Tree (S n) a
deriving instance Show a => Show (Tree n a)

data Heap (n :: Nat) a where
   H :: Heap n a
   (:=:) :: Maybe (Tree n a) -> Heap (S n) a -> Heap n a
deriving instance Show a => Show (Heap n a)

empty :: Heap (S Z) a
empty = H

meld :: (Ord a) => Tree n a -> Tree n a -> Tree (S n) a
meld t1@(N a cs) t2@(N b ds) =
   if a < b
      then N a (t2 :-: cs)
      else N b (t1 :-: ds)

merge :: (Ord a) => Heap n a -> Heap n a -> Heap n a
merge q H = q
merge H q = q
merge (Nothing :=: r1) (f2 :=: r2) =
   f2 :=: merge r1 r2
merge (f1 :=: r1) (Nothing :=: r2) =
   f1 :=: merge r1 r2
merge (Just f1 :=: r1) (Just f2 :=: r2) =
   Nothing :=: insertTree (meld f1 f2) (merge r1 r2)

insertTree :: Ord a => Tree n a -> Heap n a -> Heap n a
insertTree t h = merge (Just t :=: H) h

insert :: (Ord a) => a -> Heap (S Z) a -> Heap (S Z) a
insert a h = insertTree (N a E) h

root :: Tree n a -> a
root (N a _) = a

roots :: Heap n a -> [a]
roots H = []
roots (Nothing :=: rest) = roots rest
roots (Just x :=: rest) = root x : roots rest

findMin :: (Ord a) => Heap n a -> Maybe a
findMin h = case roots h of
   [] -> Nothing
   xs -> Just $ minimum xs

clean :: Heap n a -> Heap n a
clean = maybe H id . maybeClean
   where
      maybeClean :: Heap n a -> Maybe (Heap n a)
      maybeClean H = Nothing
      maybeClean (Nothing :=: rest) =
         (Nothing :=:) <$> maybeClean rest
      maybeClean (f :=: rest) =
         Just $ maybe (f :=: H) (f :=:) (maybeClean rest)

selectChildrenOf :: (Ord a) => a -> Heap n a -> Maybe (Heap (S Z) a, Heap n a)
selectChildrenOf _ H = Nothing
selectChildrenOf v (mf :=: rest) =
   case mf of
      Just f | v == root f ->
         let newHeap = case f of (N _ cs) -> heapFromStraight cs
         in  Just (newHeap, Nothing :=: clean rest)
      otherwise -> do
         (s,r) <- selectChildrenOf v rest
         return (s, mf :=: r)

heapFromStraight :: Straight n a -> Heap (S Z) a
heapFromStraight s = iter s H
   where
      iter :: Straight n a -> Heap (S n) a -> Heap (S Z) a
      iter E            h = h
      iter (f :-: rest) h = iter rest (Just f :=: h)

extractMin :: (Ord a) => Heap (S Z) a -> Maybe (a, Heap (S Z) a)
extractMin h = do
   m        <- findMin h
   (sel,cs) <- selectChildrenOf m h
   return (m, merge sel cs)

deleteMin :: (Ord a) => Heap (S Z) a -> Heap (S Z) a
deleteMin h = maybe h snd (extractMin h)

fromList :: (Ord a) => [a] -> Heap (S Z) a
fromList = foldr insert H

toList :: (Ord a) => Heap (S Z) a -> [a]
toList = unfoldr extractMin

heapSort :: (Ord a) => [a] -> [a]
heapSort = toList . fromList
