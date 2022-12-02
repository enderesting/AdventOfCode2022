{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Environment (getArgs)
import Data.List.Split (splitOn)

main :: IO()
main = do
        args <- getArgs -- text file name included in args
        content <- readFile $ head args
        let roundList = splitOn "\n" content
        let part1 = map (\x -> getScore x + getShape (last x)) roundList
        let part2 = map (\x -> getScore' (last x) + getShape' x) roundList
        print $ sum part1
        print $ sum part2
        return ()

getScore :: String -> Int
getScore x
    | x `elem` ["A X","B Y","C Z"] = 3 -- tie
    | x `elem` ["A Z","B X","C Y"] = 0 -- lose
    | x `elem` ["A Y","B Z","C X"] = 6 -- win
    | otherwise = error x

getShape :: Char -> Int
getShape x
    | x == 'X' = 1 -- rock
    | x == 'Y' = 2 -- paper
    | x == 'Z' = 3 -- scissors
    | otherwise = error "weird: " x

getScore' :: Char -> Int
getScore' x 
    | x == 'X' = 0 -- lose
    | x == 'Y' = 3 -- draw
    | x == 'Z' = 6 -- win

getShape' :: String -> Int
getShape' x 
    | x `elem` ["A Y","B X","C Z"] = 1
    | x `elem` ["A Z","B Y","C X"] = 2
    | x `elem` ["A X","B Z","C Y"] = 3
