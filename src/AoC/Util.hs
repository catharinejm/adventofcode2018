module AoC.Util where

import           Control.Monad.IO.Class
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import           Text.Read (readMaybe)

readLines :: (MonadIO m) => FilePath -> m [Text]
readLines file = liftIO $ T.lines <$> T.readFile file

sumLines :: [Text] -> Int
sumLines = foldl' dosum 0
  where
    dosum n l = case (T.signed T.decimal) l of
                  Right (i, _) -> n + i
                  Left _ -> n
