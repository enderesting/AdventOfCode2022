import System.Environment (getArgs)
import Data.List.Split (splitOn,splitOneOf,chunksOf,wordsBy)
import Data.Char (isAlpha,isDigit,digitToInt)
import Data.List (transpose,concat)
import Debug.Trace
debug = flip trace

------------------------------------------------------------------------------------------------
-- NOTES
-- sometimes.... foldr.... are not as good as just iterating through a list 
-- ^ because equals in this case can match onto repeating empty slots, so its best to reach them through index

-- wordsBy :: (a->Bool) -> [a] -> [[a]] is a useful equation to use a equation to split a string
-- its ALWAYS always better to write shorter and more functions than to write one big one
------------------------------------------------------------------------------------------------

main :: IO()
main = do
        args <- getArgs -- text file name included in args
        content <- readFile $ head args
        let contentLines = splitOn "\n" content
        let sectionParts = splitOn [""] contentLines
        let crates = map (concatMap $ filter isAlpha) (transpose eachLine)
                where cratesData = head sectionParts
                      eachLine = map (chunksOf 4) cratesData
        let instructions = map (map getNum) listStrInstr -- ["121"]
                where sentenceInstr = last sectionParts -- ["move 1 from 2 to 1"]
                      listStrInstr = map (wordsBy isAlpha) sentenceInstr -- [["1","2","1"]]
                      getNum x = read x :: Int
        let finalCrates = foldl oneMove crates instructions
        print $ map head finalCrates

oneMove :: [String] -> [Int] -> [String]
oneMove crates instr = [if x == (a-1) then drop n fromStack
                        else if x == (b-1) then dropCrates ++ toStack
                        else crates !! x
                        | x <- [0..length crates-1]]
                where n = head instr
                      a = instr !! 1
                      b = instr !! 2
                      fromStack = crates !! (a-1)
                      toStack = crates !! (b-1)
                      dropCrates = take n fromStack
