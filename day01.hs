main :: IO ()
main = do
  input <- readFile "input1.txt"
  print
    . map (\(x, y, z) -> x*y*z)
    . filter (\(x, y, z) -> x+y+z == 2020)
    . (\ns -> [(x, y, z) |
               x <- ns, y <- ns, z <- ns,
               x /= y, y /= z, x /= z])
    . map read
    . words $ input

    -- Replace three lines above with code below for first part
    -- . map (\(x, y) -> x*y)
    -- . filter (\(x,y) -> x+y == 2020)
    -- . (\ns -> [(x,y) | x <- ns, y <- ns, x /= y])
