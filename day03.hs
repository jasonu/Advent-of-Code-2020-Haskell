type Grid = [String]
type Slope = (Int,Int)
type Pos = (Int,Int)

dimensions :: Grid -> (Int, Int)
dimensions grid = (length (head grid), length grid)

mul :: Int -> Slope -> Pos
mul n (x,y) = ((n*x), (n*y))

getCoord :: Grid -> Pos -> Char
getCoord grid (x, y) = (grid !! v) !! u
  where
    (width,height) = dimensions grid
    (u,v) = (x `mod` width, y `mod` height)

countTrees :: Grid -> Slope -> Int
countTrees grid (x,y) = (length
                        . filter (== '#')
                        . map (getCoord grid)
                        . map (\n -> mul n (x,y))) [1..((height `div` y)-1)]
  where
    (_, height) = dimensions grid

main :: IO Int
main = do
  input <- readFile "input3.txt"
  let grid  = lines input
      path1 = countTrees grid (1,1)
      path2 = countTrees grid (3,1)
      path3 = countTrees grid (5,1)
      path4 = countTrees grid (7,1)
      path5 = countTrees grid (1,2)
  return (path1*path2*path3*path4*path5)
