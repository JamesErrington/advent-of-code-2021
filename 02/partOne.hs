type Pos = (Int, Int)

commands :: [String]
commands = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

forward :: Int -> Pos -> Pos
forward x (h, d) = (h + x, d)

down :: Int -> Pos -> Pos
down x (h, d) = (h, d + x)

up :: Int -> Pos -> Pos
up x (h, d) = (h, d - x)

parseCommand :: Pos -> String -> Pos
parseCommand pos input
  | command == "forward" = forward x pos
  | command == "down" = down x pos
  | command == "up" = up x pos
  | otherwise = pos
  where
    parsed = words input
    command = head parsed
    x = read $ last parsed

result :: Pos -> Int
result (h, d) = h * d

main :: IO ()
main = readFile "02/data.txt" >>= (print . result) . foldl parseCommand (0, 0) . lines
