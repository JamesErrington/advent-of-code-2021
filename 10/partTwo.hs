import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.State (State, get, runState, state)
import Data.Either (isRight)
import Data.List (sort)

data Bracket = Open | Close

type Stack = [Char]

type StackMonad = ExceptT Char (State Stack)

bracket :: Char -> Bracket
bracket x = if x `elem` ['(', '[', '{', '<'] then Open else Close

matching :: Char -> Char -> Bool
matching '(' ')' = True
matching '[' ']' = True
matching '{' '}' = True
matching '<' '>' = True
matching _ _ = False

score :: Char -> Int
score '(' = 1
score '[' = 2
score '{' = 3
score '<' = 4
score _ = 0

pop :: State Stack Char
pop = state $ \(x : xs) -> (x, xs)

push :: Char -> State Stack ()
push x = state $ \xs -> ((), x : xs)

parseLine :: String -> StackMonad ()
parseLine [] = return ()
parseLine (x : xs) = case bracket x of
  Open -> lift (push x) >> parseLine xs
  Close -> do
    last <- lift pop
    if matching last x
      then parseLine xs
      else throwError x

runParser :: String -> (Either Char (), Stack)
runParser x = runState (runExceptT (parseLine x)) ""

nonCorrupted :: [String] -> [Stack]
nonCorrupted = fmap snd . filter (isRight . fst) . fmap runParser

incomplete :: [String] -> [Stack]
incomplete = filter (not . null) . nonCorrupted

calculateScore :: String -> Int
calculateScore = foldl (\a e -> (a * 5) + score e) 0

main :: IO ()
main = do
  input <- lines <$> readFile "10/data.txt"
  let scores = sort $ calculateScore <$> incomplete input
  print $ scores !! (length scores `div` 2)
