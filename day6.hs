import System.Environment (getArgs)
import Data.List (nub)
import Debug.Trace
debug = flip trace

-- Set.fromList :: Ord a => [a] -> Set a 
--      ...is also useful in this scenario. i love sets

-- AOC guys said damn give this girl a break. THANK YOU!!!!! Completed around 30 minutes

main :: IO()
main = do
        args <- getArgs -- text file name included in args
        content <- readFile $ head args
        print $ markerAt content 0 4
        print $ markerAt content 0 14
        return ()

checkDistinct :: String -> Int -> Bool
checkDistinct xs n = length (nub $ take n xs) == n

markerAt :: String -> Int -> Int -> Int 
markerAt xs acc n = if checkDistinct xs n 
                    then acc+n 
                    else markerAt (tail xs) (acc+1) n
