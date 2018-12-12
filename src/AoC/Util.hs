{-# LANGUAGE TemplateHaskell #-}

module AoC.Util where

import           Control.Lens
import           Control.Lens.TH
import           Data.Attoparsec.Text

import           AoC.Prelude
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

data Square = Square { _squareNumber :: Int
                     , _squareLeft   :: Int
                     , _squareTop    :: Int
                     , _squareWidth  :: Int
                     , _squareHeight :: Int
                     }
  deriving (Eq, Ord, Show)
makeFields ''Square

parseSquare :: Parser Square
parseSquare = Square <$  char '#'
                     <*> decimal
                     <*  skipSpace
                     <*  char '@'
                     <*  skipSpace
                     <*> decimal
                     <*  char ','
                     <*> decimal
                     <*  char ':'
                     <*  skipSpace
                     <*> decimal
                     <*  char 'x'
                     <*> decimal

parseSquares :: Text -> Either String [Square]
parseSquares = parseOnly ((many' (parseSquare <* skipSpace)) <* endOfInput)

countUsages :: [Square] -> Map (Int, Int) Int
countUsages = foldr count M.empty
  where
    count sq m = foldr (M.alter bump) m (mkpairs sq)
    bump Nothing = Just 1
    bump (Just n) = Just (n+1)
    mkpairs sq = [(x, y) | x <- [sq^.left..(sq^.left + sq^.width - 1)]
                         , y <- [sq^.top..(sq^.top + sq^.height - 1)]]
