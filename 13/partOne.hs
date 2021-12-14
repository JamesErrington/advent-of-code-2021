import Data.Bifunctor (first, second)
import qualified Data.Set as S (Set, fromList, map, partition, size, union)

type Point = (Int, Int)

type Points = S.Set Point

data Fold = X Int | Y Int deriving (Show)

performFold :: Fold -> Points -> Points
performFold (X n) = xFold n
performFold (Y n) = yFold n

xFold :: Int -> Points -> Points
xFold n points = S.union right (S.map (first ((2 * n) -)) left)
  where
    (left, right) = S.partition ((< n) . fst) points

yFold :: Int -> Points -> Points
yFold n points = S.union above (S.map (second ((2 * n) -)) below)
  where
    (above, below) = S.partition ((> n) . snd) points

parsePoint :: String -> Point
parsePoint x = read $ "(" ++ x ++ ")"

parseFold :: String -> Fold
parseFold (a : '=' : n)
  | a == 'y' = Y $ read n
  | otherwise = X $ read n
parseFold _ = undefined

main :: IO ()
main = do
  file <- lines <$> readFile "13/data.txt"
  let (ps, fs) = fmap (drop 11) <$> (tail <$> break (== "") file)
  let points = S.fromList $ parsePoint <$> ps
  let folds = parseFold <$> fs
  print $ S.size $ performFold (head folds) points
