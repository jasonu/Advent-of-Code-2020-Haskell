module Main where

import Data.List(nub)

split :: [String] -> [[String]]
split ss = split' ss []

split' :: [String] -> [String]-> [[String]]
split' [] acc = [acc] -- acc is short for accumulator
split' (s:ss) acc | null s = acc : (split' ss [])
                  | otherwise = split' ss (acc ++ [s])

main :: IO ()
main = do
  input <- readFile "input06.txt"
  print
    . sum
    . map length
    . map nub
    . map (\ss -> concat ss)
    . split
    . lines $ input
