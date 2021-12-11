indexes :: (Int, Int) -> Int -> [Int]
indexes (width, height) i = up ++ down ++ left ++ right
  where
    up = [i - width | i >= width]
    down = [i + width | i < width * (height - 1)]
    left = [i - 1 | i `mod` width > 0]
    right = [i + 1 | (i + 1) `mod` width > 0]

values :: [Int] -> [Int] -> [Int]
values map = fmap (map !!)

lowpoints :: [Int] -> (Int, Int) -> [Int]
lowpoints map wh = (map !!) <$> filter (\i -> all (\j -> map !! i < j) ((values map . indexes wh) i)) [0 .. length map - 1]

score :: [Int] -> Int
score = foldr ((+) . (+ 1)) 0

main :: IO ()
main = do
  input <- lines <$> readFile "09/data.txt"
  let (width, height) = (length $ head input, length input)
  print $ score $ lowpoints [read [x] | x <- concat input] (width, height)
