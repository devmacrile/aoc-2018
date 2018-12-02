import Data.Set (Set)
import qualified Data.Set as Set

parseFreq :: String -> Int
parseFreq x = if ((head x) == '+') then read (tail x) else read x

partOne :: [String] -> Int
partOne xs = sum (map parseFreq xs)

partTwo :: [String] -> Int
partTwo xs = repeatFreq (map parseFreq xs) Set.empty 0

repeatFreq :: [Int] -> Set Int -> Int -> Int
repeatFreq (x:xs) s y = if (Set.member y s) then y else repeatFreq (xs ++ [x]) (Set.insert y s) (y + x)

main = do
        contents <- readFile "input.txt"
        let ops = lines contents
        print ("Part one = " ++ show (partOne ops))
        print ("Part two = " ++ show (partTwo ops))

