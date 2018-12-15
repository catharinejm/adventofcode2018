module AoC.Days.Types where

import AoC.Prelude
import Control.Lens.TH

data Day01 = Day01 { _day01FilePath :: FilePath
                   , _day01Deltas :: [Int]
                   }
  deriving (Show)
makeFields ''Day01

data Day02 = Day02 { _day02FilePath :: FilePath
                   , _day02BoxIDs :: [Text]
                   }
  deriving (Show)
makeFields ''Day02
