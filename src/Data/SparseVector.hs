{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Data.SparseVector
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.SparseVector
  ( -- * Sparse vectors
    SparseVector (..),

    -- * Construction
    empty,

    -- ** Operations
    insert,
    lookup,
    delete,
    mapWithKey,
    mapAccum,

    -- ** Intersection
    intersection,
    intersectionWith,
    intersectionWithKey,
    intersectionVec,
    intersectionVecWith,
    intersectionVecWithKey,

    -- ** Conversion
    fromList,
    toList,
    fromVector,
    toVector,

    -- ** Mutations
    freeze,
    unsafeFreeze,
    thaw,
    unsafeThaw,
  )
where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Strict
import Data.Maybe
import Data.SparseVector.Mutable (MSparseVector (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (PrimMonad (..))
import Prelude hiding (lookup)

-- | Sparse n-dimensional vector.
newtype SparseVector a = SparseVector {unSparseVector :: Vector (Bool, a)}
  deriving (Show, Eq, NFData)

instance Functor SparseVector where
  fmap f (SparseVector v) = SparseVector $ V.map (\(present, val) -> (present, f val)) v
  {-# INLINE fmap #-}

instance Foldable SparseVector where
  foldr f acc (SparseVector v) = V.foldr (\(present, val) acc' -> if present then f val acc' else acc') acc v
  {-# INLINE foldr #-}

-- | Empty sparse vector (requires a default value for operations that need it).
empty :: SparseVector a
empty = SparseVector V.empty
{-# INLINE empty #-}

-- | Insert an element at a given index into a `SparseVector`.
insert :: Int -> a -> SparseVector a -> SparseVector a
insert index a (SparseVector vec) =
  let len = V.length vec
   in SparseVector $
        if len >= index + 1
          then V.unsafeUpd vec [(index, (True, a))]
          else V.snoc (vec V.++ V.replicate (index - len) (False, undefined)) (True, a)
{-# INLINE insert #-}

-- | Lookup an element at a given index in a `SparseVector`.
lookup :: Int -> SparseVector a -> Maybe a
lookup i (SparseVector v) =
  case v V.!? i of
    Just (True, val) -> Just val
    _ -> Nothing
{-# INLINE lookup #-}

-- | Delete an index from a `SparseVector`, replacing its cell with @(False, defaultVal)@.
delete :: Int -> SparseVector a -> SparseVector a
delete index (SparseVector vec) =
  SparseVector $ V.unsafeUpd vec [(index, (False, undefined))]
{-# INLINE delete #-}

mapWithKey :: (Int -> a -> b) -> SparseVector a -> SparseVector b
mapWithKey f (SparseVector v) =
  let go (i, (present, val)) = (present, if present then f i val else undefined)
   in SparseVector (go <$> V.indexed v)
{-# INLINE mapWithKey #-}

mapAccum :: (a -> b -> (a, c)) -> a -> SparseVector b -> (a, SparseVector c)
mapAccum f a (SparseVector v) =
  let f' (True, b) = do
        acc <- get
        let (acc', c) = f acc b
        put acc'
        return (True, c)
      f' (False, _) = return (False, undefined)
      (v', a') = runState (V.mapM f' v) a
   in (a', SparseVector v')

intersection :: SparseVector a -> SparseVector b -> SparseVector a
intersection sv1 sv2 = intersectionWith const sv1 sv2

intersectionWith :: (a -> b -> c) -> SparseVector a -> SparseVector b -> SparseVector c
intersectionWith f = intersectionWithKey (const f)

intersectionWithKey :: (Int -> a -> b -> c) -> SparseVector a -> SparseVector b -> SparseVector c
intersectionWithKey f (SparseVector a) (SparseVector b) =
  let (as, bs) =
        if V.length a >= V.length b
          then (a, b V.++ V.replicate (V.length a - V.length b) (False, undefined))
          else (a V.++ V.replicate (V.length b - V.length a) (False, undefined), b)
      go (i, ((True, a'), (True, b'))) = (True, f i a' b')
      go _ = (False, undefined)
   in SparseVector . fmap go . V.indexed $ V.zip as bs

intersectionVec :: SparseVector a -> SparseVector b -> Vector a
intersectionVec = intersectionVecWith const
{-# INLINE intersectionVec #-}

intersectionVecWith :: (a -> b -> c) -> SparseVector a -> SparseVector b -> Vector c
intersectionVecWith = intersectionVecWithKey . const
{-# INLINE intersectionVecWith #-}

intersectionVecWithKey :: (Int -> a -> b -> c) -> SparseVector a -> SparseVector b -> Vector c
intersectionVecWithKey f (SparseVector a) (SparseVector b) = V.imapMaybe go $ V.zip a b
  where
    go i ((True, a'), (True, b')) = Just $ f i a' b'
    go _ _ = Nothing
{-# INLINE intersectionVecWithKey #-}

fromList :: [(Int, a)] -> SparseVector a
fromList xs = foldr (\(i, a) -> insert i a) empty xs

toList :: SparseVector a -> [Maybe a]
toList (SparseVector v) = V.toList $ V.map (\(present, val) -> if present then Just val else Nothing) v
{-# INLINE toList #-}

fromVector :: Vector a -> SparseVector a
fromVector v = SparseVector $ V.map (True,) v

toVector :: SparseVector a -> Vector a
toVector (SparseVector v) = V.mapMaybe (\(present, val) -> if present then Just val else Nothing) v

-- | Freeze a `MSparseVector` into a `SparseVector`.
freeze :: (PrimMonad m) => MSparseVector (PrimState m) a -> m (SparseVector a)
freeze (MSparseVector vec) = do
  vec' <- V.freeze vec
  return $ SparseVector vec'
{-# INLINE freeze #-}

-- | Unfreeze a `SparseVector` into a `MSparseVector`.
thaw :: (PrimMonad m) => SparseVector a -> m (MSparseVector (PrimState m) a)
thaw (SparseVector vec) = do
  vec' <- V.thaw vec
  return $ MSparseVector vec'
{-# INLINE thaw #-}

-- | Freeze a `MSparseVector` into a `SparseVector`.
unsafeFreeze :: (PrimMonad m) => MSparseVector (PrimState m) a -> m (SparseVector a)
unsafeFreeze (MSparseVector vec) = do
  !vec' <- V.unsafeFreeze vec
  return $ SparseVector vec'
{-# INLINE unsafeFreeze #-}

-- | Unfreeze a `SparseVector` into a `MSparseVector`.
unsafeThaw :: (PrimMonad m) => SparseVector a -> m (MSparseVector (PrimState m) a)
unsafeThaw (SparseVector vec) = do
  !vec' <- V.unsafeThaw vec
  return $ MSparseVector vec'
{-# INLINE unsafeThaw #-}
