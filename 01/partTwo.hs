depths :: [Int]
depths = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

tripleSums :: [Int] -> [Int]
tripleSums ds = zipWith3 (\x y z -> x + y + z) (tail $ tail ds) (tail ds) ds

count :: [Int] -> Int
count ds = length $ filter (> 0) $ zipWith (-) (tail ds) ds

main :: IO ()
main = readFile "01/data.txt" >>= (print . count . tripleSums) . map read . lines
