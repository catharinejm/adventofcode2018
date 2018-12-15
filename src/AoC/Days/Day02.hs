module AoC.Days.Day02 where

import           Control.Lens
import           Control.Lens.TH

import           AoC.Days.Types
import           AoC.Prelude
import qualified AoC.Prelude.Map as M
import qualified AoC.Prelude.Text as T
import           AoC.Util

initDay02 :: (MonadIO m) => FilePath -> m Day02
initDay02 path = Day02 path <$> readLines path

day02Part1 :: (MonadIO m, MonadReader Day02 m) => m Int
day02Part1 = do
  freqs <- map frequencies <$> view boxIDs
  let exactlyTwos = countTwos freqs
      exactlyThrees = countThrees freqs
  return (exactlyTwos * exactlyThrees)
  where
    countTwos = foldr (incIf 2) 0
    countThrees = foldr (incIf 3) 0
    incIf n m s = (M.foldr (oneIf n) 0 m) + s
    oneIf n ((== n) -> True) _ = 1
    oneIf _ _ r = r


day02Part2 :: (MonadIO m, MonadReader Day02 m) => m Text
day02Part2 = do
  ids <- view boxIDs
  let (id1, id2) = offByOnes ids
  return (T.pack (sameLetters id1 id2))
  where
    offByOnes [] = error "no ids off by one found"
    offByOnes (id:ids) = case find (offByOne id) ids of
                           Just id' -> (id, id')
                           Nothing -> offByOnes ids
    offByOne id1 id2 = foldr cmp 0 (id1 `T.zip` id2) == 1
    cmp (c1, c2) n = if c1 /= c2 then n+1 else n
    sameLetters id1 id2 = foldr (\(c1, c2) ls -> if c1 == c2 then c1:ls else ls) [] (id1 `T.zip` id2)

day02 :: (MonadIO m) => m (Int, Text)
day02 = do
  initDay02 "./files/day2.txt" >>= runReaderT ((,) <$> day02Part1 <*> day02Part2)
