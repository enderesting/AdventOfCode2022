{-
"i just woke up and stared at the problem for 10 min" approach:
okay two parts to this problem:
1. find out the stepping sequence of T using H's
    preferably parsed in String
2. on a large enough grid (lets sa 100x100) run these steps from the middle
3. sum.filter out tiles that are walked on???

final approach:
1. find out [coords] to the head
2. based on headCoords, a function to find out tailCoords
    part2. make it repeatable! find last of that [[coords]]
3. nub out repeated Coords, count all

thoughts:
- hey guess who just remembered you can comment out blocks
- this is the first time I had to rewrite one whole function after
    completing part 1 :( sad! oh well, at least it wasn't too bad
- the most time i've spent on today was on step 2:
    how to logically detect when tail should move according to head 
- i tried very hard to avoid using arithmetics to calculate tail position. 
    ended up having to pay for part 2 anyways. :/
- here's an messy sketch of my thought process for today's puzzle:
    https://imgur.com/IgWLcEA
-}

import System.Environment (getArgs)
import Debug.Trace (trace)
import Data.List (nub)

debug :: c -> String -> c
debug = flip trace

type Maze = [String]
type Coords = (Int,Int)
type Step = (String, Int)

main :: IO()
main = do
        args <- getArgs -- text file name included in args
        content <- readFile $ head args
        let headSteps = [ (x, read y ::Int)  | (x,y) <- charTuple]
                where charTuple = map (splitAt 1) (lines content)
        let headPath = tail $ headCoords headSteps
        let tailPath = tailCoords headPath
    -- part 1 --
        -- print headPath
        -- print tailPath
        print $ length $ nub tailPath
    -- part 2 --
        let ropePath = take 10 $ iterate tailCoords headPath 
            -- ^returns a list, each item is a [Coords] to each rope node
        let ropeTailPath = last ropePath
        -- print $ ropeTailPath
        print $ length $ nub ropeTailPath

-- STEP 1, parsing head --
headCoords :: [Step] -> [Coords]
headCoords = foldl (\acc x@(dir,num) -> acc ++ steps x (last acc)) [(0,0)]
        where steps (dir,num) (x,y) = tail $ reverse $ foldr (\x acc -> findDest x (head acc): acc) [(x,y)] (replicate num dir)

-- Aux of headCoords, for readability
direction :: String -> Coords
direction c
        | c == "D" = (0,-1)
        | c == "U" = (0,1)
        | c == "L" = (-1,0)
        | c == "R" = (1,0)
        | otherwise = error "Oops! Not an available direction."

-- stolen directly from my haskell PP project
findDest :: String -> Coords -> Coords
findDest c (a,b)= (fst dir + a, snd dir + b)
                    where dir = direction c

-- STEP 2, finding [coords] of tail --
-- [OG tailCoords I used for part 1] --
{-
tailCoords :: [Coords] -> [Coords] -- -> [(Coords,Coords)] -- 
tailCoords xs = tail $ map fst $ scanl (\acc@(tailPos, lastHead) x@headPos 
-- tailCoords xs = scanl (\acc@(tailPos, lastHead) x@headPos 
                                        -> if outOfRange tailPos headPos --`debug` ("out: " ++ show tailPos ++ show (outOfRange tailPos headPos))
                                           then (lastHead, headPos) -- if out range
                                           else (tailPos, headPos)  -- if in range: tail stay, 
                                    ) ((0,0),head xs) xs
                                                -- x: head's most recent step
                                                -- acc: tailPos, last step for head
-}

-- given the list of coords head went to, calculate the list of coords for tail
tailCoords :: [Coords] -> [Coords] -- -> [(Coords,Coords)] -- 
tailCoords xs = tail $ scanl (\acc@tailPos x@headPos 
                                        -> if outOfRange tailPos headPos
                                           then moveTo tailPos headPos -- if out range, move
                                           else tailPos  -- if in range: stay
                                    ) (0,0) xs
                                                -- x: head's most recent step
                                                -- acc: tailPos

-- Aux functions for tailCoords: 
-- evaluate where the head is, and move there accordingly
moveTo :: Coords -> Coords -> Coords
moveTo tail@(a1,b1) head@(a2,b2) = (a1 + eval (a2-a1), b1 + eval (b2-b1))

-- figure out where to move
eval :: Int -> Int -- given (2,-1) -> (1,-1)
eval n 
    | n > 0 = 1
    | n < 0 = -1
    | otherwise = 0

-- if head move to any of the surround coords to tail's original position
-- tail does not move. this tests if head's still in that range
outOfRange :: Coords -> Coords -> Bool
outOfRange tail@(a1,b1) head@(a2,b2) = abs (a2-a1) >= 2 || abs (b2- b1) >= 2
