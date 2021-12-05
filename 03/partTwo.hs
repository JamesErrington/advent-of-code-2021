binary :: [String]
binary = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

matchCriteria :: (Int, Int) -> ((Int, Int) -> Bool) -> String -> Char
matchCriteria zo pred []
  | pred zo = '0'
  | otherwise = '1'
matchCriteria (z, o) pred (c : cs)
  | c == '0' = matchCriteria (z + 1, o) pred cs
  | otherwise = matchCriteria (z, o + 1) pred cs

findMostCommon :: String -> Char
findMostCommon = matchCriteria (0, 0) (uncurry (>))

findLeastCommon :: String -> Char
findLeastCommon = matchCriteria (0, 0) (uncurry (<=))

rotate :: [String] -> Int -> String
rotate xs i = fmap (!! i) xs

filterCandidates :: [String] -> Int -> (String -> Char) -> [String]
filterCandidates xs i criteria = filter (\x -> x !! i == mask) xs
  where
    mask = criteria (rotate xs i)

findValue :: [String] -> Int -> (String -> Char) -> String
findValue xs i criteria
  | length xs == 1 = head xs
  | otherwise = findValue (filterCandidates xs i criteria) (i + 1) criteria

toDecimal :: String -> Int -> Int
toDecimal [] n = 0
toDecimal (c : cs) n = 2 ^ n * read [c] + toDecimal cs (n + 1)

lifeSupport :: [String] -> Int
lifeSupport bs = toDecimal oxygen 0 * toDecimal co2 0
  where
    oxygen = reverse $ findValue bs 0 findMostCommon
    co2 = reverse $ findValue bs 0 findLeastCommon

main :: IO ()
main = readFile "03/data.txt" >>= print . lifeSupport . lines
