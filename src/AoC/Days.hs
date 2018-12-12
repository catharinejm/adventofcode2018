module AoC.Days ( module AoC.Days
                , module AoC.Days.Day01
                ) where

import           AoC.Days.Day01
import           AoC.Prelude
import qualified AoC.Prelude.Map as M
import qualified AoC.Prelude.Text as T
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

day03 :: (MonadIO m) => m (Either String Int)
day03 = do
  text <- liftIO $ T.readFile "./files/day3.txt"
  case parseSquares text of
    Left err -> return $ Left err
    Right squares -> do
      let usages = countUsages squares
      return $ Right (overused usages)
  where
    overused = M.foldr (\u s -> if u > 1 then s+1 else s) 0
