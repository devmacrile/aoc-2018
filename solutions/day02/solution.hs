once :: String -> Char -> Bool
once [] _ = False
once (c:cs) x = ((c == x) && not (elem x cs)) || (c /= x && once cs x)

twice :: String -> Char -> Bool
twice [] _ = False
twice (c:cs) x = ((c == x) && once cs x) || (c /= x && twice cs x)

hasPair :: String -> Bool
hasPair [] = False
hasPair (l:ls) = once ls l || hasPair (filter (\x -> x /= l) ls)

hasTriplet :: String -> Bool
hasTriplet [] = False
hasTriplet (l:ls) = twice ls l || hasTriplet (filter (\x -> x /= l) ls)

numSatisfying :: [String] -> (String -> Bool) -> Int
numSatisfying x f = length (filter (\x -> x) (map f x))

numPairs :: [String] -> Int
numPairs x = numSatisfying x hasPair

numTriplets :: [String] -> Int
numTriplets x = numSatisfying x hasTriplet

stringDiff :: String -> String -> Int
stringDiff [] y = 0
stringDiff x [] = 0
stringDiff (x:xs) (y:ys) = if (x == y) then (stringDiff xs ys) else (1 + stringDiff xs ys)

stringIntersect :: String -> String -> String
stringIntersect [] y = []
stringIntersect x [] = []
stringIntersect (x:xs) (y:ys) = if (x == y) then [x] ++ (stringIntersect xs ys) else stringIntersect xs ys

allPairs :: [a] -> [b] -> [(a, b)]
allPairs xs ys = [(x,y) | x <- xs, y <- ys]

stringDiffs :: [(String, String)] -> [(String, String, Int)]
stringDiffs xs = map (\(x,y) -> (x, y, stringDiff x y)) xs

closestPair :: [(String, String, Int)] -> (String, String)
closestPair xs = head (map (\(x, y, z) -> (x, y)) (filter (\(x, y, z) -> z == 1) xs))

bestIntersection :: (String, String) -> String
bestIntersection (x, y) = stringIntersect x y

partTwo :: [String] -> String
partTwo xs = bestIntersection (closestPair (stringDiffs (allPairs xs xs))) 

main = do
        contents <- readFile "input.txt"
        let boxIds = lines contents
        print ("Part 1 = " ++ show ((numPairs boxIds), (numTriplets boxIds), (numPairs boxIds) * (numTriplets boxIds)))
        print ("Part 2 = " ++ show (partTwo boxIds))
        
