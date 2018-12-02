module AoC.Days.Day01 where

import Control.Monad.IO.Class

import AoC.Util

day01 :: (MonadIO m) => m Int
day01 = sumLines <$> readLines "./files/day1.txt"
  
