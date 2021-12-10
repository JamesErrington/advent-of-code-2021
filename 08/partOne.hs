count :: [String] -> Int
count = length . filter ((`elem` [2, 3, 4, 7]) . length)

parseLines :: String -> [[String]]
parseLines = fmap (words . dropWhile (/= '|')) . lines

main :: IO ()
main = do
  input <- parseLines <$> readFile "08/data.txt"
  print $ foldr ((+) . count) 0 input
