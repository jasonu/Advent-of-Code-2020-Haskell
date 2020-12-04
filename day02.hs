type Password = String
type Policy = (Int, Int, Char)

checkPasswordA :: (Policy, Password) -> Bool
checkPasswordA ((l, u, c), pass) = (cnt >= l) && (cnt <= u)
  where cnt = length . filter (== c) $ pass

checkPasswordB :: (Policy, Password) -> Bool
checkPasswordB ((m, n, c), pass) = a /= b
  where
    a = (pass !! (m-1)) == c
    b = (pass !! (n-1)) == c

split :: String -> Char -> [String]
split str delimiter = split' str delimiter []

split' :: String -> Char -> [Char] -> [String]
split' [] _ acc = [acc] -- acc is short for accumulator
split' (c:cs) del acc | c == del  = acc : (split' cs del [])
                      | otherwise = split' cs del (acc ++ [c])

parseRange :: String -> (Int,Int)
parseRange s = (left, right)
  where
    (m:n:[]) = split s '-'
    left     = read m :: Int
    right    = read n :: Int

parseLine :: [String] -> (Policy, Password)
parseLine [] = error "Empty line"
parseLine (_:[]) = error "Missing letter in policy"
parseLine (_:_:[]) = error "Missing password"
parseLine (range:letter:pass) = ((m, n, c), p)
  where
     (m,n) = parseRange range
     c     = head letter
     p     = head pass

main :: IO ()
main = do
  input <- readFile "input2.txt"
  print
    . length
    . filter (== True)
    . map checkPasswordB
    . map parseLine
    . map words
    . lines $ input
