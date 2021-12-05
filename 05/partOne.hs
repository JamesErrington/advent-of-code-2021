import qualified Data.List as L
import qualified Data.Text as T

type Point = (Int, Int)

type Line = (Point, Point)

testLines :: [Line]
testLines = [((0, 9), (5, 9)), ((8, 0), (0, 8)), ((9, 4), (3, 4)), ((2, 2), (2, 1)), ((7, 0), (7, 4)), ((6, 4), (2, 0)), ((0, 9), (2, 9)), ((3, 4), (1, 4)), ((0, 0), (8, 8)), ((5, 5), (8, 2))]

coveredPoints :: Line -> [Point]
coveredPoints ((x1, y1), (x2, y2))
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
  | otherwise = []

findCoveredPoints :: [Line] -> [[Point]]
findCoveredPoints lines = (L.group . L.sort) $ lines >>= coveredPoints

count :: [[Point]] -> Int
count = length . filter (\x -> length x > 1)

result :: [Line] -> Int
result = count . findCoveredPoints

parseLine :: String -> Line
parseLine input = ((readX start, readY start), (readX end, readY end))
  where
    points = T.splitOn (T.pack " -> ") (T.pack input)
    start = T.splitOn (T.pack ",") (head points)
    end = T.splitOn (T.pack ",") (last points)
    readX = read . T.unpack . head
    readY = read . T.unpack . last

parseLines :: IO [Line]
parseLines = do
  file <- readFile "05/data.txt"
  let rows = lines file
  return $ fmap parseLine rows

main :: IO ()
main = do
  lines <- parseLines
  print $ result lines
