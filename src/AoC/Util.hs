module AoC.Util where

import AoC.Prelude
import qualified AoC.Prelude.Map as M
import qualified AoC.Prelude.Text as T

readLines :: (MonadIO m) => FilePath -> m [Text]
readLines file = liftIO $ T.lines <$> T.readFile file

sumLines :: [Text] -> Int
sumLines = foldl' dosum 0
  where
    dosum n l = case (T.signed T.decimal) l of
                  Right (i, _) -> n + i
                  Left _ -> n

frequencies :: Text -> Map Char Int
frequencies = T.foldl' sumfreqs M.empty
  where
    sumfreqs m c = M.alter bump c m
    bump (Just n) = Just (n+1)
    bump Nothing = Just 1
