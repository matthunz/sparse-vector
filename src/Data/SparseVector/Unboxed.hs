{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.SparseVector.Unboxed
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.SparseVector.Unboxed
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

    -- ** Functor-like operations (with Unbox constraints)
    svMap,

    -- ** Foldable-like operations (with Unbox constraints)
    svFoldr,
    svFoldl,
    svLength,
    svNull,

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
    toVec,

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
import Data.SparseVector.Unboxed.Mutable (MSparseVector (..))
import Data.Vector.Mutable (PrimMonad (..))
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import Prelude hiding (lookup)

-- | Sparse n-dimensional vector using unboxed vectors.
newtype SparseVector a = SparseVector {unSparseVector :: Vector (Bool, a)}

-- Standalone deriving instances
deriving instance (Show a, Unbox a) => Show (SparseVector a)

deriving instance (Eq a, Unbox a) => Eq (SparseVector a)

deriving instance (Unbox a) => NFData (SparseVector a)

-- | Map a function over a SparseVector (requires Unbox constraints)
svMap :: (Unbox a, Unbox b) => (a -> b) -> SparseVector a -> SparseVector b
svMap f (SparseVector v) = SparseVector $ V.map (\(present, val) -> (present, f val)) v
{-# INLINE svMap #-}

-- | Fold a SparseVector from the right (requires Unbox constraints)
svFoldr :: (Unbox a) => (a -> b -> b) -> b -> SparseVector a -> b
svFoldr f acc (SparseVector v) = V.foldr (\(present, val) acc' -> if present then f val acc' else acc') acc v
{-# INLINE svFoldr #-}

-- | Fold a SparseVector from the left (requires Unbox constraints)
svFoldl :: (Unbox a) => (b -> a -> b) -> b -> SparseVector a -> b
svFoldl f acc (SparseVector v) = V.foldl (\acc' (present, val) -> if present then f acc' val else acc') acc v
{-# INLINE svFoldl #-}

-- | Get the number of actual elements (non-empty cells) in a SparseVector
svLength :: (Unbox a) => SparseVector a -> Int
svLength (SparseVector v) = V.length (V.filter fst v)
{-# INLINE svLength #-}

-- | Check if a SparseVector has no actual elements
svNull :: (Unbox a) => SparseVector a -> Bool
svNull (SparseVector v) = V.all (not . fst) v
{-# INLINE svNull #-}

-- | Empty sparse vector.
empty :: (Unbox a) => SparseVector a
empty = SparseVector V.empty
{-# INLINE empty #-}

-- | Insert an element at a given index into a `SparseVector`.
--
-- Inserting elements at some dimension @n@ will grow the vector up to @n@,
-- using @(False, a)@ to create empty cells (where @a@ is the inserted value).
--
-- >>> insert 0 'a' empty
-- SparseVector {unSparseVector = [(True, 'a')]}
--
-- >>> insert 2 'b' empty
-- SparseVector {unSparseVector = [(False, 'b'),(False, 'b'),(True, 'b')]}
insert :: (Unbox a) => Int -> a -> SparseVector a -> SparseVector a
insert index a (SparseVector vec) =
  let len = V.length vec
   in SparseVector $
        if len >= index + 1
          then V.unsafeUpd vec [(index, (True, a))]
          else V.snoc (vec V.++ V.replicate (index - len) (False, a)) (True, a)
{-# INLINE insert #-}

-- | Lookup an element at a given index in a `SparseVector`.
lookup :: (Unbox a) => Int -> SparseVector a -> Maybe a
lookup i (SparseVector v) =
  case v V.!? i of
    Just (True, val) -> Just val
    _ -> Nothing
{-# INLINE lookup #-}

-- | Delete an index from a `SparseVector`, marking its cell as not present.
delete :: (Unbox a) => Int -> SparseVector a -> SparseVector a
delete index (SparseVector vec) =
  let (_, existingVal) = vec V.! index
   in SparseVector $ V.unsafeUpd vec [(index, (False, existingVal))]
{-# INLINE delete #-}

mapWithKey :: (Unbox a, Unbox b) => (Int -> a -> b) -> SparseVector a -> SparseVector b
mapWithKey f (SparseVector v) =
  let indexed = zip [0 ..] (V.toList v)
      go (i, (present, val)) = (present, f i val)
   in SparseVector $ V.fromList $ map go indexed
{-# INLINE mapWithKey #-}

mapAccum :: (Unbox b, Unbox c) => (a -> b -> (a, c)) -> a -> SparseVector b -> (a, SparseVector c)
mapAccum f a (SparseVector v) =
  let f' (True, b) = do
        acc <- get
        let (acc', c) = f acc b
        put acc'
        return (True, c)
      f' (False, b) = do
        acc <- get
        let (_, c) = f acc b
        return (False, c)
      vecList = V.toList v
      (resultList, a') = runState (mapM f' vecList) a
   in (a', SparseVector (V.fromList resultList))

intersection :: (Unbox a, Unbox b) => SparseVector a -> SparseVector b -> SparseVector a
intersection sv1 sv2 = intersectionWith const sv1 sv2

intersectionWith :: (Unbox a, Unbox b, Unbox c) => (a -> b -> c) -> SparseVector a -> SparseVector b -> SparseVector c
intersectionWith f = intersectionWithKey (const f)

intersectionWithKey :: (Unbox a, Unbox b, Unbox c) => (Int -> a -> b -> c) -> SparseVector a -> SparseVector b -> SparseVector c
intersectionWithKey f (SparseVector a) (SparseVector b) =
  let minLen = min (V.length a) (V.length b)
      as = V.take minLen a
      bs = V.take minLen b
      go (i, ((True, a'), (True, b'))) = (True, f i a' b')
      go (i, ((_, a'), (_, b'))) = (False, f i a' b') -- Use f to produce a valid c value
      pairs = zip [0 ..] $ zip (V.toList as) (V.toList bs)
   in SparseVector $ V.fromList $ map go pairs

intersectionVec :: (Unbox a, Unbox b) => SparseVector a -> SparseVector b -> Vector a
intersectionVec = intersectionVecWith const
{-# INLINE intersectionVec #-}

intersectionVecWith :: (Unbox a, Unbox b, Unbox c) => (a -> b -> c) -> SparseVector a -> SparseVector b -> Vector c
intersectionVecWith = intersectionVecWithKey . const
{-# INLINE intersectionVecWith #-}

intersectionVecWithKey :: (Unbox a, Unbox b, Unbox c) => (Int -> a -> b -> c) -> SparseVector a -> SparseVector b -> Vector c
intersectionVecWithKey f (SparseVector a) (SparseVector b) =
  let go i (True, a') (True, b') = Just $ f i a' b'
      go _ _ _ = Nothing
      results = zipWith3 go [0 ..] (V.toList a) (V.toList b)
   in V.fromList $ catMaybes results
{-# INLINE intersectionVecWithKey #-}

fromList :: (Unbox a) => [(Int, a)] -> SparseVector a
fromList xs = foldr (\(i, a) -> insert i a) empty xs

toList :: (Unbox a) => SparseVector a -> [Maybe a]
toList (SparseVector v) = map (\(present, val) -> if present then Just val else Nothing) $ V.toList v
{-# INLINE toList #-}

fromVector :: (Unbox a) => Vector a -> SparseVector a
fromVector v = SparseVector $ V.map (\x -> (True, x)) v

toVec :: (Unbox a) => SparseVector a -> Vector a
toVec (SparseVector v) = V.fromList $ mapMaybe (\(present, val) -> if present then Just val else Nothing) $ V.toList v

-- | Freeze a `MSparseVector` into a `SparseVector`.
freeze :: (PrimMonad m, Unbox a) => MSparseVector (PrimState m) a -> m (SparseVector a)
freeze (MSparseVector vec) = do
  vec' <- V.freeze vec
  return $ SparseVector vec'
{-# INLINE freeze #-}

-- | Unfreeze a `SparseVector` into a `MSparseVector`.
thaw :: (PrimMonad m, Unbox a) => SparseVector a -> m (MSparseVector (PrimState m) a)
thaw (SparseVector vec) = do
  vec' <- V.thaw vec
  return $ MSparseVector vec'
{-# INLINE thaw #-}

-- | Freeze a `MSparseVector` into a `SparseVector`.
unsafeFreeze :: (PrimMonad m, Unbox a) => MSparseVector (PrimState m) a -> m (SparseVector a)
unsafeFreeze (MSparseVector vec) = do
  !vec' <- V.unsafeFreeze vec
  return $ SparseVector vec'
{-# INLINE unsafeFreeze #-}

-- | Unfreeze a `SparseVector` into a `MSparseVector`.
unsafeThaw :: (PrimMonad m, Unbox a) => SparseVector a -> m (MSparseVector (PrimState m) a)
unsafeThaw (SparseVector vec) = do
  !vec' <- V.unsafeThaw vec
  return $ MSparseVector vec'
{-# INLINE unsafeThaw #-}
