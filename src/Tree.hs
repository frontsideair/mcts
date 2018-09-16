{-# LANGUAGE ScopedTypeVariables #-}

module Tree where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Semigroup

data Tree k v = Node v (Map k (Tree k v)) deriving Show
-- TODO: Consider using IOArray here

rootLabel :: Tree k v -> v
rootLabel (Node v _) = v

subForest :: Tree k v -> [(k, Tree k v)]
subForest (Node _ vs) = M.toList vs

node :: v -> Tree k v
node v = Node v M.empty

newtype Snd a b = Snd { _unSnd :: (a, b) } deriving Show

instance Ord b => Semigroup (Snd a b) where
  Snd (a,b) <> Snd (a',b') = if b >= b' then Snd (a, b) else Snd (a', b')

findMax :: Ord b => (t -> b) -> Map a t -> Maybe (a, t)
findMax f m = fst . _unSnd <$> M.foldMapWithKey g m
  where g k v = Just $ Snd ((k, v), f v)

insert :: Ord k => k -> a -> Map k a -> Map k a
insert = M.insert
