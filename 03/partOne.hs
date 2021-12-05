binary :: [String]
binary = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

mostCommon :: String -> (Int, Int) -> String
mostCommon [] (z, o)
  | z > o = "0"
  | otherwise = "1"
mostCommon (c : cs) (z, o)
  | c == '0' = mostCommon cs (z + 1, o)
  | c == '1' = mostCommon cs (z, o + 1)
  | otherwise = ""

rotate :: [String] -> Int -> [String] -> [String]
rotate xs i bs
  | i == (length . head) xs = bs
  | otherwise = rotate xs (i + 1) (mostCommon (fmap (!! i) xs) (0, 0) : bs)

toDecimal :: [String] -> Int -> Int
toDecimal [] n = 0
toDecimal (x : xs) n = 2 ^ n * read x + toDecimal xs (n + 1)

invert :: [String] -> [String]
invert [] = []
invert (b : bs)
  | b == "0" = "1" : invert bs
  | otherwise = "0" : invert bs

power :: [String] -> Int
power bs = toDecimal gamma 0 * toDecimal epsilon 0
  where
    gamma = rotate bs 0 []
    epsilon = invert gamma

main = readFile "03/data.txt" >>= print . power . lines
