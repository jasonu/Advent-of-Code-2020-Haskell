module Main where

import Data.List(sort)

bsp2bits :: Char -> Int
bsp2bits c = case c of
               'F' -> 0
               'B' -> 1
               'L' -> 0
               'R' -> 1
               _   -> 0

bits2int :: [Int] -> Int
bits2int = foldl (\h l -> 2*h + l) 0

seatID :: (Int, Int) -> Int
seatID (row, col) = row*8 + col

findEmptySeat :: [Int] -> Int
findEmptySeat [] = (-1)
findEmptySeat (n:ns) = findEmptySeat' n ns

findEmptySeat' :: Int -> [Int] -> Int
findEmptySeat' _ [] = (-1)
findEmptySeat' prev (n:ns) = if (n /= (prev+1))
                            then
                              (prev+1)
                            else
                              findEmptySeat' n ns

main :: IO ()
main = do
  input <- readFile "input05.txt"
  print
    -- . foldr max 0
    . findEmptySeat
    . sort
    . map seatID
    . map (\(row, col) -> (bits2int row, bits2int col))
    . map (\s -> (take 7 s, drop 7 s))
    . map (\s -> map bsp2bits s)
    . lines $ input
