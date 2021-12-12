import Data.List (unfoldr)

type Octopus = Int

type Index = (Int, Int)

chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not . null) . unfoldr (Just . splitAt n)

indexes :: Int -> [Index]
indexes size = [(x, y) | x <- [0 .. size - 1], y <- [0 .. size - 1]]

neighbours :: Int -> Index -> [Index]
neighbours size (x, y) = upleft ++ up ++ upright ++ left ++ right ++ downleft ++ down ++ downright
  where
    up = [(x - 1, y) | x > 0]
    down = [(x + 1, y) | x < size - 1]
    left = [(x, y - 1) | y > 0]
    right = [(x, y + 1) | y < size - 1]
    upleft = [(x - 1, y - 1) | x > 0, y > 0]
    upright = [(x - 1, y + 1) | x > 0, y < size - 1]
    downleft = [(x + 1, y - 1) | x < size - 1, y > 0]
    downright = [(x + 1, y + 1) | x < size - 1, y < size - 1]

value :: [[Octopus]] -> Index -> Octopus
value grid (x, y) = (grid !! x) !! y

clampGrid :: Int -> [[Octopus]] -> [[Octopus]]
clampGrid size grid = chunk size [if x > 9 then 0 else x | xs <- grid, x <- xs]

step :: Int -> (Int, [[Octopus]]) -> (Int, [[Octopus]])
step size (fs, grid) = clampGrid size <$> step' size (fmap (fmap (+ 1)) grid) []

step' :: Int -> [[Octopus]] -> [Index] -> (Int, [[Octopus]])
step' size grid fs
  | null flashes = (length fs, grid)
  | otherwise = step'' size grid flashes (flashes ++ fs)
  where
    flashes = [i | i <- indexes size, (grid `value` i) > 9, i `notElem` fs]

step'' :: Int -> [[Octopus]] -> [Index] -> [Index] -> (Int, [[Octopus]])
step'' size grid [] = step' size grid
step'' size grid (i : is) = step'' size updated is
  where
    updated = chunk size [if j `elem` neighbours size i then (grid `value` j) + 1 else grid `value` j | j <- indexes size]

main :: IO ()
main = do
  file <- lines <$> readFile "11/data.txt"
  let size = length $ head file
  let height = length file
  let input = chunk size [read [x] | x <- concat file]
  print $ length $ takeWhile (\(n, _) -> n < size * height) (iterate (step size) (0, input))
