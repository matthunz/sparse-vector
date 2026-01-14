module Main where

import qualified Data.SparseVector.Unboxed as SV

main :: IO ()
main = print . SV.lookup 10 . SV.insert 10 (2 :: Int) $ SV.insert 0 1 SV.empty
