-- i keep saying this to myself but: SOMETIMES!!! LIST IS BETTER!!!!
-- i will never learn! i will never learn!!!!!!! *cling onto foldr structure n yell*

import System.Environment (getArgs)
import Data.List(transpose)
import Debug.Trace (trace)

debug :: c -> String -> c
debug = flip trace

type Tree = Int
type Index = Int

main :: IO()
main = do
        args <- getArgs -- text file name included in args
        content <- readFile $ head args
        let maze = [[read [i]| i <- eachLine] | eachLine <- lines content]
      -- alt:  let maze = map strToNum (lines content)
        let transMaze = transpose maze
        let row = length maze -1
        let col = length transMaze -1
        let checkForTrue = [ horVis (maze!!i) j || horVis (transMaze!!j) i
                            | i <- [0..row], j <- [0..col]]  --`debug` ("row:"++show row++"    col:" ++show col)
        let checkScenic  = [ horSce (maze!!i) j * horSce (transMaze!!j) i
                            | i <- [0..row], j <- [0..col]]  --`debug` ("row:"++show row++"    col:" ++show col)
        print $ length $ filter (==True) checkForTrue
        print $ maximum checkScenic

-- --mapping a string to a list of Int
-- strToNum :: String -> [Int]
-- strToNum = map toNum
--         where toNum c = read [c] :: Int

-- thanks star for the function B]
-- for edge cases, split returns empty lists -> returning -1
maximum' :: [Int] -> Int
maximum' list
            | null list = -1
            | otherwise = maximum list

-- part 1
horVis :: [Tree] -> Index -> Bool
horVis mazeLine index = num > leftMax || num > rightMax --`debug` ("left:"++ show leftMax ++"  right:"++show rightMax)
                            where (left,right) = splitAt index mazeLine 
                                  leftMax = maximum' left
                                  rightMax = maximum' (tail right) 
                                  num = mazeLine !! index

-- part 2
horSce :: [Tree] -> Index -> Int
horSce mazeLine index = leftScore * rightScore --`debug` ("checking:"++ show num ++"left:" ++ show leftScore ++ "right" ++ show rightScore)
                            where (left,right) = splitAt index mazeLine 
                                  leftScore = checkScene (reverse left) num 
                                  rightScore = checkScene (tail right) num 
                                  num = mazeLine !! index

-- aux function, split out to look prettier
-- checks the score given list of Ints representing trees in front of a tree n
checkScene :: [Tree] -> Tree -> Int 
--                      ^note this Int is the tree's value, not index
checkScene xs n = if seenCount < length xs 
                then seenCount+1 
                else seenCount
            where seenTrees = takeWhile (<n) xs
                  seenCount = length seenTrees
