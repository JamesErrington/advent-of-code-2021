import qualified Data.Text as T

positions :: [Int]
positions = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

fuelCost :: Int -> Int -> Int
fuelCost x y = abs (x - y)

parseNumbers :: String -> [Int]
parseNumbers input = read . T.unpack <$> T.splitOn (T.pack ",") (T.pack input)

main :: IO ()
main = do
  file <- readFile "07/data.txt"
  let numbers = parseNumbers file
  print $ minimum $ fmap (\x -> sum $ fmap (fuelCost x) numbers) [0 .. maximum numbers]
