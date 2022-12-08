-- insp: ephemient
-- read their code and tried to implement it in my own way. point is learning!
-- and what i learnt is that sometimes... not everything is as it seem (aka sometimes trying to make a Tree structure at 6am is just impossible)
-- and the best data structure for said data may not be the fastest nor easiest to implement, and in puzzles like this?
-- sometimes quick and dirty is the way to go. lmao

-- stripPrefix              : taking away the prefix, leaving Maybe String
-- Just x <- Maybe x        : good way to grab Just values in guards
-- Data.Map                 : key-value pairs. fun data structure i love u Map
-- (elems, insertWith...)
-- foldl'                   : better choice than foldl

import System.Environment (getArgs)
import Data.List (nub, stripPrefix, tails, foldl', isPrefixOf, sort)
import qualified Data.Map as Map
import Data.Char (isDigit)
import Data.Map (singleton)

import Debug.Trace (trace)
debug :: c -> String -> c
debug = flip trace

main :: IO()
main = do
        args <- getArgs -- text file name included in args
        content <- readFile $ head args
        let commands = lines content
        let finalRecord = foldl' readCommand (Records [] (singleton [] 0)) commands
        let sizeMap = Map.elems $ dirMap finalRecord
        print $ part1 sizeMap
        print $ part2 sizeMap
        return ()

-- acc (crumbs, dirMap)
        -- crumbs -> a list that changes whenever a cd command is called -- points to current location
        -- dirMap -> a map keeps track of size, mapped for every directory (including crumbs)
data Records = Records {crumbs :: [String], dirMap :: Map.Map [String] Size}
type Name = String
type Size = Int

-- cmd one line at a time
type Command = String

-- updates record every line of command
readCommand :: Records -> Command -> Records
readCommand record@(Records crumbs dirMap) cmd
        | cmd == "$ cd /" = Records [] dirMap -- move pointer to the beginning
        | cmd == "$ cd .." = Records (tail crumbs) dirMap -- move pointer one back, rest the same
        | Just dir <- "$ cd " `stripPrefix` cmd = Records (dir:crumbs) dirMap -- step into the directory list
        |  "$ ls" `isPrefixOf` cmd || "dir " `isPrefixOf` cmd = record 
        -- ideally i dont need these lines... 
        -- can't seem to evaluate AND capture the num in the next line, though  
        | cmd /= "" = Records crumbs (updateSize record $ toInt $ checkNum cmd) -- cmd points to file, updates dirMap
        | otherwise = record -- if anything else, returns just the record
            where toInt xs = read xs --`debug` ("xs is"++xs) :: Int --day7.hs AdventOfCode2022
                  checkNum size = filter isDigit size
--NOTES:
-- if "dir a" -> ignore, record maintains
    -- assumes all dir are visited to gather the size
-- if "12312 a.txt" -> readCmd and add size to the trail of directories (locatedAt) that includes this file
    -- assumes you only cd into the same folder ONE time. <<<<< important!!!
-- if "$ ls" -> ignore

updateSize ::   Records -> --old record including trail and map
                Size -> -- size of file to be added to every dir
                Map.Map [String] Size -- returns new map
updateSize (Records locatedAt map) size = foldl' updateOne map listOfDirs
                            where updateOne map dir = Map.insertWith (+) dir size map
                                  listOfDirs = tails locatedAt -- generates a list of dirs 
                                --   note that listOfDirs includes one branch of nested folders

-- total file size files <= 100000
part1 :: [Int] -> Int
part1 xs = sum $ filter (<= 100000) xs

-- 70000000 - (used - dirToDelete) >= 30000000
-- delete the smallest file possible
part2 :: [Int] -> Int
part2 xs = minimum (filter (\x -> 40000000 >= stillUses x) xs)
        where stillUses x = head xs - x
