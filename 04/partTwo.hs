import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Text as T

type Row = [(Int, Bool)]

type Board = [Row]

numbers :: [Int]
numbers = parseNumbers "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"

board1 :: [String]
board1 = ["22 13 17 11 0", "8 2 23 4 24", "21 9 14 16 7", "6 10 3 18 5", "1 12 20 15 19"]

board2 :: [String]
board2 = ["3 15 0 2 22", "9 18 13 17 5", "19 8 7 25 23", "20 11 10 24 4", "14 21 16 12 6"]

board3 :: [String]
board3 = ["14 21 17 24 4", "10 16 15 9 19", "18 8 23 26 20", "22 11 13 6 5", "2 0 12 3 7"]

boards :: [Board]
boards = fmap parseBoard [board1, board2, board3]

parseNumbers :: String -> [Int]
parseNumbers input = read . T.unpack <$> T.splitOn (T.pack ",") (T.pack input)

parseRow :: String -> Row
parseRow row = [(read x, False) | x <- words row]

parseBoard :: [String] -> Board
parseBoard = fmap parseRow

parseBoards :: [String] -> [Board] -> [Board]
parseBoards ("" : a : b : c : d : e : "" : rest) boards = parseBoards rest (parseBoard [a, b, c, d, e] : boards)
parseBoards (a : b : c : d : e : "" : rest) boards = parseBoards rest (parseBoard [a, b, c, d, e] : boards)
parseBoards [a, b, c, d, e] boards = parseBoard [a, b, c, d, e] : boards
parseBoards _ boards = boards

markBoard :: Int -> Board -> Board
markBoard n = fmap (fmap $ markNumber n)
  where
    markNumber i (x, m)
      | m || x == i = (x, True)
      | otherwise = (x, False)

checkBoard :: Board -> Either Board Board
checkBoard board
  | isDone = Left board
  | otherwise = Right board
  where
    isDone = or (fmap (any (all snd)) [board, L.transpose board])

computeScore :: Board -> Int
computeScore = foldl (\a e -> a + sum [x | (x, y) <- e, not y]) 0

lastBingo :: [Int] -> [Board] -> Either Int [Board]
lastBingo [] boards = Right boards
lastBingo (n : ns) [lastBoard] = case nextStep of
  Left finishedBoard -> Left $ n * computeScore finishedBoard
  Right nextBoard -> lastBingo ns [nextBoard]
  where
    nextStep = (checkBoard . markBoard n) lastBoard
lastBingo (n : ns) boards = lastBingo ns unfinished
  where
    unfinished = E.rights $ fmap (checkBoard . markBoard n) boards

main :: IO ()
main = do
  file <- readFile "04/data.txt"
  let contents = lines file
  let numbers = parseNumbers $ head contents
  let boards = parseBoards (tail contents) []
  print $ lastBingo numbers boards
