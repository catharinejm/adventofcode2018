module Main where

import AoC.Days

main :: IO ()
main = do
  res01 <- day01
  putStrLn   "Advent of Code 2018"
  putStrLn $ "Day 1: " ++ show res01
