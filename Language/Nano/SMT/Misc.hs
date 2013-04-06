{-# LANGUAGE DeriveDataTypeable, TupleSections, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Language.Nano.SMT.Misc where

sortNub :: (Ord a) => [a] -> [a]
sortNub = nubOrd . L.sort

nubOrd (x:t@(y:_)) 
  | x == y    = nubOrd t 
  | otherwise = x : nubOrd t
nubOrd xs     = xs

sortNubBy :: (a -> a -> Ordering) -> [a] -> [a]
sortNubBy cmp = nubOrd . L.sortBy cmp



