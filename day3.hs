import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust)

main :: IO()
main = do
        args <- getArgs -- text file name included in args
        content <- readFile $ head args
        let contentLines = splitOn "\n" content
        let sackLines = map (\x -> splitAt (length x `div` 2) x) contentLines
        let groupLines = splitBy 3 contentLines
        print $ sum $ map findRepeat sackLines 
        print $ sum $ map findBadge groupLines
        
findRepeat :: (String,String) -> Int
findRepeat (fst,snd) = priorityDic $ fromJust x
        where x = find (`elem` fst) snd
             
findBadge :: [String] -> Int
findBadge xs = priorityDic $ fromJust badge 
        where possibleBadges = filter (`elem` head xs) (last xs)
              badge = find (`elem` possibleBadges) (head $ tail xs)

priorityDic :: Char -> Int
priorityDic x = fromJust $ lookup x dic
        where dic = zip (['a'..'z']++['A'..'Z']) [1..62] 

splitBy :: Int -> [a] -> [[a]]
splitBy n [] = []
splitBy n xs = take n xs : splitBy n (drop n xs)
