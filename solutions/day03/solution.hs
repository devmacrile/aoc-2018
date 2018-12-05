import Data.Map (fromListWith, toList)
import Text.Regex.Posix

type Pos = (Int, Int)
data Claim = Claim { claimId :: Int, pos :: Pos, width :: Int, height :: Int} deriving Show

claimPositions :: Claim -> [Pos]
claimPositions (Claim _ (x, y) w h) = [(i, j) | i <- [x .. (x + w - 1)], j <- [y .. (y + h - 1)]]

posCounts :: [Pos] -> [(Pos, Int)]
posCounts ps = toList (fromListWith (+) [(p, 1) | p <- ps])

partOne :: [Claim] -> Int
partOne cs = length (filter (\(_,x) -> x > 1) (posCounts (cs >>= claimPositions)))

overlap :: Claim -> Claim -> Bool
overlap (Claim _ (x1, y1) w1 h1) (Claim _ (x2, y2) w2 h2) = not ((x1 + w1 - 1 < x2) || (x1 > x2 + w2 - 1) || (y1 + h1 - 1 < y2) || (y1 > y2 + h2 - 1))

overlaps :: Claim -> [Claim] -> Bool
overlaps c cs = or (map (\x -> if (claimId c == claimId x) then False else overlap c x) cs)

partTwo :: [Claim] -> Claim
partTwo cs = head (filter (\x -> not (overlaps x cs)) cs)

readInt :: String -> Int
readInt s = read s :: Int
 
parseClaim :: String -> Claim
parseClaim s = Claim id (x, y) w h
                 where [id, x, y, w, h] = map readInt (getAllTextMatches $ s =~ "([0-9]+)" :: [String])

main = do
        contents <- readFile "input.txt"
        let claims = map parseClaim (lines contents)
        print (head claims)
        print ("Part one = " ++ show (partOne claims))
        print ("Part two = " ++ show (partTwo claims))
