
module Underload.Util(foldShows, mpow) where

foldShows :: [ShowS] -> ShowS
foldShows = foldr (.) id

mpow :: Monoid m => m -> Int -> m
mpow x n = mconcat $ replicate n x
