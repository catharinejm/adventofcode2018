module AoC.Days.Day01 where

import           Control.Lens
import           Control.Lens.TH

import           AoC.Days.Types
import           AoC.Prelude
import qualified AoC.Prelude.Set as S
import qualified AoC.Prelude.Text as T
import           AoC.Util

initDay01 :: (MonadIO m) => FilePath -> m Day01
initDay01 path = Day01 path <$> (readLines path >>= return . map toint)
  where
    toint s = case (T.signed T.decimal) s of
                Right (i, _) -> i
                Left _ -> error . show $ s <> " is not an integer"

day01Part1 :: (MonadIO m, MonadReader Day01 m) => m Int
day01Part1 = sum <$> view deltas

day01Part2 :: (MonadIO m, MonadReader Day01 m) => m Int
day01Part2 = findRepeat S.empty 0 <$> (cycle <$> view deltas)
  where
    findRepeat _ _ [] = error "no deltas"
    findRepeat seen n (d:ds) = if n `S.member` seen
                               then n
                               else findRepeat (S.insert n seen) (n+d) ds

day01 :: (MonadIO m) => m (Int, Int)
day01 = initDay01 "./files/day1.txt" >>= runReaderT ((,) <$> day01Part1 <*> day01Part2)
