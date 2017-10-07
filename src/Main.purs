module Data.ChapterFour where

import Data.Foldable
import Prelude
import Data.Path
import Data.Array

import Control.MonadZero (guard)
import Data.Maybe (isJust)

square arr = map (\n -> n * n) arr
removeNegatives arr = filter (\n -> n > -1) arr

pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)

factors n = filter (
  \pair -> product pair == n
  ) (pairs n)

factorsDo :: Int -> Array (Array Int)
factorsDo n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  pure [i, j]

factorsGuard :: Int -> Array (Array Int)
factorsGuard n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

fact :: Int -> Int -> Int
fact 0 acc = acc
fact n acc = fact (n - 1) (acc * n)

isPrime :: Int -> Boolean
isPrime n = n `mod` 2 /= 0

allFiles :: Path -> Array Path
allFiles file = file : do
  child <- ls file
  allFiles child

