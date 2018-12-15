module AoC.Days.Types where

import AoC.Prelude
import Control.Lens.TH

-- Day 1
data Day01 = Day01 { _day01FilePath :: FilePath
                   , _day01Deltas :: [Int]
                   }
  deriving (Show)
makeFields ''Day01

-- Day 2
data Day02 = Day02 { _day02FilePath :: FilePath
                   , _day02BoxIDs :: [Text]
                   }
  deriving (Show)
makeFields ''Day02

-- Day 3
data Day03 = Day03 { _day03FilePath :: FilePath
                   , _day03Squares :: [Square]
                   }

data Square = Square { _squareNumber :: Int
                     , _squareLeft   :: Int
                     , _squareTop    :: Int
                     , _squareWidth  :: Int
                     , _squareHeight :: Int
                     }
  deriving (Eq, Ord, Show)

makeFields ''Day03
makeFields ''Square
