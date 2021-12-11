import Data.List (sortBy)
import Data.Set (fromList, size)

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
lowpoints map wh = filter (\i -> all (\j -> map !! i < j) ((values map . indexes wh) i)) [0 .. length map - 1]

basin :: [Int] -> (Int, Int) -> [Int] -> [Int] -> [Int]
basin _ _ _ [] = []
basin map wh ps (i : is) = i : basin map wh (i : ps) (ns ++ is)
  where
    ns = [x | x <- indexes wh i, map !! x < 9, x `notElem` ps]

main :: IO ()
main = do
  input <- lines <$> readFile "09/data.txt"
  let (width, height) = (length $ head input, length input)
  let map = [read [x] | x <- concat input]
  let ls = lowpoints map (width, height)
  let basins = basin map (width, height) [] . pure <$> ls
  print $ product $ take 3 $ sortBy (flip compare) (size . fromList <$> basins)
