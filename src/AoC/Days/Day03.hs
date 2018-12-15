module AoC.Days.Day03 where

import           AoC.Days.Types
import           AoC.Prelude
import qualified AoC.Prelude.Map as M
import qualified AoC.Prelude.Text as T
import           AoC.Util

import           Control.Lens

initDay03 :: (MonadIO m) => FilePath -> m Day03
initDay03 path = do
  text <- liftIO (T.readFile path)
  case parseSquares text of
    Left err -> error err
    Right squares -> return (Day03 path squares)

day03Part1 :: (MonadReader Day03 m) => m Int
day03Part1 = do
  view squares >>= return . overused . countUsages
  where
    overused = M.foldr (\u s -> if u > 1 then s+1 else s) 0

day03 :: (MonadIO m) => m Int
day03 = do
  day03 <- initDay03 "./files/day3.txt"
  runReaderT day03Part1 day03
