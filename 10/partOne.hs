import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.State (State, get, runState, state)

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
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
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

calculateScore :: String -> Int
calculateScore = either score (const 0) . fst . runParser

main :: IO ()
main = do
  input <- lines <$> readFile "10/data.txt"
  print $ foldr ((+) . calculateScore) 0 input
