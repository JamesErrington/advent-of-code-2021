import Data.Bifunctor (first, second)
import Data.Foldable (foldl', maximumBy)
import Data.List (unfoldr)
import Data.Ord (comparing)
import qualified Data.Set as S (Set, fromList, map, partition, toList, union)

type Point = (Int, Int)

type Points = S.Set Point

data Fold = X Int | Y Int deriving (Show)

performFold :: Fold -> Points -> Points
performFold (X n) = xFold n
performFold (Y n) = yFold n

xFold :: Int -> Points -> Points
xFold n points = S.union left (S.map (first (\x -> (2 * n) - x)) right)
  where
    (left, right) = S.partition ((<= n) . fst) points

yFold :: Int -> Points -> Points
yFold n points = S.union above (S.map (second (\y -> (2 * n) - y)) below)
  where
    (above, below) = S.partition ((<= n) . snd) points

performFolds :: [Fold] -> Points -> Points
performFolds fs points = foldl' (flip performFold) points fs

indexes :: Point -> [Point]
indexes (x, y) = [(i, j) | i <- [0 .. x], j <- [0 .. y]]

chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not . null) . unfoldr (Just . splitAt n)

printPaper :: [Point] -> IO ()
printPaper points = mapM_ (print . foldl' (\a e -> (if e `elem` points then "#" else ".") ++ a) "") grid
  where
    maxX = fst $ maximumBy (comparing fst) points
    maxY = snd $ maximumBy (comparing snd) points
    grid = chunk (maxY + 1) $ indexes (maxX, maxY)

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
  let paper = performFolds folds points
  printPaper $ S.toList paper
