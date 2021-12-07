import qualified Data.Text as T

initialFish :: [Int]
initialFish = [3, 4, 3, 1, 2]

lifeTimes :: [Int] -> [Int]
lifeTimes fs = fmap (\x -> (length . filter (== x)) fs) [0 .. 8]

step :: [Int] -> [Int]
step [x0, x1, x2, x3, x4, x5, x6, x7, x8] = [x1, x2, x3, x4, x5, x6, x7 + x0, x8, x0]
step x = x

parseNumbers :: String -> [Int]
parseNumbers input = read . T.unpack <$> T.splitOn (T.pack ",") (T.pack input)

main :: IO ()
main = do
  file <- readFile "06/data.txt"
  let numbers = lifeTimes $ parseNumbers file
  print $ sum $ (!! 256) $ iterate step numbers
