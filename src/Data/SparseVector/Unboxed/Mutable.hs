{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Data.SparseVector.Unboxed.Mutable
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.SparseVector.Unboxed.Mutable
  ( MSparseVector (..),
    empty,
    insert,
    read,
    unsafeRead,
    write,
    unsafeWrite,
    modify,
    unsafeModify,
    toList,
  )
where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector, PrimMonad (..), Unbox)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Maybe
import Prelude hiding (read)

newtype MSparseVector s a = MSparseVector {unMSparseVector :: MVector s (Bool, a)}

empty :: (PrimMonad m, Unbox a) => m (MSparseVector (PrimState m) a)
empty = do
  vec <- MV.new 0
  return $ MSparseVector vec

insert ::
  (PrimMonad m, Unbox a) =>
  Int ->
  a ->
  MSparseVector (PrimState m) a ->
  m ()
insert index a (MSparseVector vec) = do
  let len = MV.length vec
  if len >= index + 1
    then MV.write vec index (True, a)
    else do
      newVec <- MV.replicate (index + 1) (False, undefined)
      -- Copy old data
      let copyLoop i
            | i >= len = return ()
            | otherwise = do
                val <- MV.read vec i
                MV.write newVec i val
                copyLoop (i + 1)
      copyLoop 0
      MV.write newVec index (True, a)

read :: (PrimMonad m, Unbox a) => MSparseVector (PrimState m) a -> Int -> m (Maybe a)
read (MSparseVector vec) i
  | i < 0 || i >= MV.length vec = return Nothing
  | otherwise = do
      (present, val) <- MV.read vec i
      return $ if present then Just val else Nothing
{-# INLINE read #-}

unsafeRead :: (PrimMonad m, Unbox a) => MSparseVector (PrimState m) a -> Int -> m a
unsafeRead (MSparseVector vec) index = do
  (_, val) <- MV.unsafeRead vec index
  return val
{-# INLINE unsafeRead #-}

write :: (PrimMonad m, Unbox a) => MSparseVector (PrimState m) a -> Int -> Maybe a -> a -> m ()
write (MSparseVector vec) i maybeVal defaultVal
  | i < 0 || i >= MV.length vec = return () -- Out of bounds, ignore
  | otherwise = case maybeVal of
      Nothing -> MV.write vec i (False, defaultVal)
      Just val -> MV.write vec i (True, val)
{-# INLINE write #-}

unsafeWrite :: (PrimMonad m, Unbox a) => MSparseVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite (MSparseVector vec) index v = MV.unsafeWrite vec index (True, v)
{-# INLINE unsafeWrite #-}

modify :: (PrimMonad m, Unbox a) => MSparseVector (PrimState m) a -> Int -> (Maybe a -> Maybe a) -> a -> m ()
modify (MSparseVector vec) index f defaultVal
  | index < 0 || index >= MV.length vec = return () -- Out of bounds, ignore
  | otherwise = do
      !(present, val) <- MV.read vec index
      let currentVal = if present then Just val else Nothing
      case f currentVal of
        Nothing -> MV.write vec index (False, defaultVal)
        Just newVal -> MV.write vec index (True, newVal)
{-# INLINE modify #-}

unsafeModify :: (PrimMonad m, Unbox a) => MSparseVector (PrimState m) a -> Int -> (a -> a) -> m ()
unsafeModify (MSparseVector vec) index f = do
  !(present, val) <- MV.unsafeRead vec index
  if present
    then MV.unsafeWrite vec index (True, f val)
    else return ()
{-# INLINE unsafeModify #-}

toList :: (PrimMonad m, Unbox a) => MSparseVector (PrimState m) a -> m [Maybe a]
toList (MSparseVector v) = do
  pairs <- V.toList <$> V.freeze v
  return $ map (\(present, val) -> if present then Just val else Nothing) pairs
{-# INLINE toList #-}
