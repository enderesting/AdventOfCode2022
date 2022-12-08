import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (intersect)
import qualified Data.Map (intersection)

main :: IO()
main = do
        args <- getArgs -- text file name included in args
        content <- readFile $ head args
        let contentLines = splitOn "\n" content
        let pairLines = map (\x -> (head $ rangePair x,last $ rangePair x)) contentLines
                    where rangePair = map findRange.splitOn ","  -- [[a..A],[b..B]]
        print $ length $ filter findCompleteSlackers pairLines
        print $ length $ filter findSlackers pairLines

-- given a-b, return [a..b]
findRange :: String -> [Int]
findRange xs = [a..b]
            where splited = splitOn "-" xs
                  a = read $ head splited :: Int
                  b = read $ last splited :: Int

-- if intersection = one of the elves work -> this guy's sus
-- returns number of intersected pairs
findCompleteSlackers :: ([Int],[Int])-> Bool
findCompleteSlackers twoElves = uncurry intersect twoElves `elem` [fst twoElves,snd twoElves]

findSlackers :: ([Int],[Int])-> Bool
findSlackers twoElves = uncurry intersect twoElves /= []
