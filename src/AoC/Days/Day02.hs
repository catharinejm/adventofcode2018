module AoC.Days.Day02 where

import           AoC.Prelude
import qualified AoC.Prelude.Map as M
import           AoC.Util

day02 :: (MonadIO m) => m Int
day02 = do
  lines <- readLines "./files/day2.txt"
  let freqs = map frequencies lines
      exactlyTwos = countTwos freqs
      exactlyThrees = countThrees freqs
  return (exactlyTwos * exactlyThrees)
  where
    countTwos = foldr (incIf 2) 0
    countThrees = foldr (incIf 3) 0
    incIf n m s = (M.foldr (oneIf n) 0 m) + s
    oneIf n ((== n) -> True) _ = 1
    oneIf _ _ r = r
