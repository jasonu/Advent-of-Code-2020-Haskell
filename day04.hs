import Data.Char(isDigit, isHexDigit)

type KV = (String,String)
type Passport = [KV]

--- Parsing Functions

split :: [String] -> [[String]]
split ss = split' ss []

split' :: [String] -> [String]-> [[String]]
split' [] acc = [acc] -- acc is short for accumulator
split' (s:ss) acc | null s = acc : (split' ss [])
                  | otherwise = split' ss (acc ++ [s])

concatWith :: String -> [String] -> String
concatWith _ [] = []
concatWith sep (s:ss) = s ++ sep ++ (concatWith sep ss)

parseKV :: String -> KV
parseKV [] = ("err","error")
parseKV (_:[]) = ("err","error")
parseKV (_:_:[]) = ("err","error")
parseKV (_:_:_:[]) = ("err","error")
parseKV (_:_:_:_:[]) = ("err","error")
parseKV (c1:c2:c3:':':rest) = ((c1:c2:c3:[]), rest)
parseKV (_:_:_:_:_) = ("err","error")

parsePassport :: [String] -> Passport
parsePassport kvs = map parseKV kvs

--- Validation Functions

requiredFields :: [String]
requiredFields = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

contains :: String -> Passport -> Bool
contains k pass = case (lookup k pass) of
                    Nothing -> False
                    Just _  -> True

validateKV :: KV -> Bool
validateKV (k,v) = case k of
  "byr" -> validateByr v
  "iyr" -> validateIyr v
  "eyr" -> validateEyr v
  "hgt" -> validateHgt v
  "hcl" -> validateHcl v
  "ecl" -> validateEcl v
  "pid" -> validatePid v
  "cid" -> True
  _     -> False

validateByr :: String -> Bool
validateByr v = length v==4 && and (map isDigit v) && (n>=1920) && (n<=2002)
  where n = read v :: Int

validateIyr :: String -> Bool
validateIyr v = length v==4 && and (map isDigit v) && (n>=2010) && (n<=2020)
  where n = read v :: Int

validateEyr :: String -> Bool
validateEyr v = length v==4 && and (map isDigit v) && (n>=2020) && (n<=2030)
  where n = read v :: Int

validateHgt :: String -> Bool
validateHgt v = validateHgt' (reverse v)

validateHgt' :: String -> Bool
validateHgt' [] = False
validateHgt' (_:[]) = False
validateHgt' ('m':'c':rest) = and (map isDigit rest) && v>=150 && v<=193
  where v = read (reverse rest) :: Int
validateHgt' ('n':'i':rest) = and (map isDigit rest) && v>=59 && v<=76
  where v = read (reverse rest) :: Int
validateHgt' (_:_:_) = False

validateHcl :: String -> Bool
validateHcl [] = False
validateHcl ('#':rest) = length rest == 6 && and (map isHexDigit rest)
validateHcl (_:_) = False

validateEcl :: String -> Bool
validateEcl v = v=="amb" || v=="blu" || v=="brn" || v=="gry" ||
                v=="grn" || v=="hzl" || v=="oth"

validatePid :: String -> Bool
validatePid v = length v == 9 && and (map isDigit v)

validatePassport :: Passport -> Bool
validatePassport pass = requiredFieldsPresent && allFieldsValid
  where
    validators = map (\k -> contains k) requiredFields
    requiredFieldsPresent = and (apply validators pass)
    allFieldsValid = and (map validateKV pass)

apply :: [a -> b] -> a -> [b]
apply [] _ = []
apply (f:fs) a = (f a) : apply fs a

--- Pretty Printing Functions (Useful for debugging)

ppKV :: KV -> String
ppKV (k,v) = k ++ ":" ++ v ++ "\n"

ppPassport :: Passport -> String
ppPassport pass = (case (validatePassport pass) of
                      True -> "valid=TRUE\n"
                      False -> "valid=FALSE\n") ++
                  ((concat (map ppKV pass)) ++ "\n")

main :: IO ()
main = do
  input <- readFile "input4.txt"
  print
    . length
    . filter (== True)
    . map validatePassport
    . map parsePassport
    . map words
    . map (\lst -> concatWith " " lst)
    . split
    . lines $ input
