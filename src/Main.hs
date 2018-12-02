module Main where

import AoC.Prelude
import AoC.Days

main :: IO ()
main = do
  res01 <- day01
  res02 <- day02
  putStrLn   "Advent of Code 2018"
  putStrLn $ "Day 1: " ++ show res01
  putStrLn $ "Day 2: " ++ show res02
