import Data.List (elemIndex, permutations, sort, sortBy, splitAt)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

type Wiring = Map Char Char

toDigit :: String -> Char
toDigit s
  | s == "cf" = '1'
  | s == "acf" = '7'
  | s == "bcdf" = '4'
  | s == "abdfg" = '5'
  | s == "acdeg" = '2'
  | s == "acdfg" = '3'
  | s == "abcdfg" = '9'
  | s == "abdefg" = '6'
  | s == "abcdefg" = '8'
  | otherwise = '0'

toNumber :: [String] -> Int
toNumber = read . foldr (:) "" . fmap toDigit

generateWirings :: [Wiring]
generateWirings = fmap (fromList . zip "abcdefg") (permutations "abcdefg")

applyWiring :: Wiring -> String -> String
applyWiring w = sort . fmap (fromJust . flip Data.Map.lookup w)

customSorter :: String -> String -> Ordering
customSorter x y
  | length x /= length y = comparing length x y
  | otherwise = compare x y

testWiring :: [String] -> Wiring -> Bool
testWiring xs w = sortBy customSorter (fmap (applyWiring w) xs) == ["cf", "acf", "bcdf", "abdfg", "acdeg", "acdfg", "abcdfg", "abcefg", "abdefg", "abcdefg"]

parseLine :: [String] -> ([String], [String])
parseLine x = splitAt (fromJust $ elemIndex "|" x) x

main :: IO ()
main = do
  let wirings = generateWirings
  input <- lines <$> readFile "08/data.txt"
  let parsedLines = parseLine . words <$> input
  let correctWirings = fmap ((\x -> head (filter (testWiring x) wirings)) . fst) parsedLines
  let correctDigits = fmap (\(w, ds) -> fmap (applyWiring w) ds) (zip correctWirings (fmap (tail . snd) parsedLines))
  print $ sum (fmap toNumber correctDigits)
