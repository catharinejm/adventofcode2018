module AoC.Days ( module AoC.Days
                , module AoC.Days.Day01
                , module AoC.Days.Day02
                ) where

import           AoC.Days.Day01
import           AoC.Days.Day02
import           AoC.Prelude
import qualified AoC.Prelude.Map as M
import qualified AoC.Prelude.Text as T
import           AoC.Util

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
