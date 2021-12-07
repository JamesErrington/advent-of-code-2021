import qualified Data.Text as T

initialFish :: [Int]
initialFish = [3, 4, 3, 1, 2]

step :: [Int] -> [Int]
step = concatMap (\x -> if x == 0 then [6, 8] else [x - 1])

parseNumbers :: String -> [Int]
parseNumbers input = read . T.unpack <$> T.splitOn (T.pack ",") (T.pack input)

main :: IO ()
main = do
  file <- readFile "06/data.txt"
  let numbers = parseNumbers file
  print $ length $ (!! 80) $ iterate step numbers
