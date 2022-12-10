{-
addx V -> takes 2 cycles to complete. register X change after the second cycle
noop -> takes 1 cycle to complete

check X every 20, and then every 40 cycles after it
[20,60,100..] ->  [x | x <- [20..length sth], (x-20) `mod` 40 == 0]

- I think the harder part was reading and understanding the puzzle...
    and syncing instructions to clocks!
- It's hard when someone's brandomly monologuing into your ear okay
- I'll probably update with a more optimal solution once I check out how the pros
    do it. Not too proud with this but it gets the job done!
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Environment (getArgs)
import Debug.Trace (trace)
import Data.List (stripPrefix,foldl')
import Data.List.Split (chunksOf)

debug :: c -> String -> c
debug = flip trace

type RegisterCount = Int
type Clock = Int

main :: IO()
main = do
        args <- getArgs -- text file name included in args
        content <- readFile $ head args
        let instructions = lines content
        let cycleStates = foldl' (\acc x -> acc ++ syncCycle (last acc) x) [(1,0)] instructions
                        -- n = 1,2,3.. clock counting, represents during the nth cycle
                        -- n = 0,1,2.. represents the end of nth cycle
        let checkSignals = [ x | x <- [20..length cycleStates], (x-20) `mod` 40 == 0]
        let signalValues = map (\x -> fst $ cycleStates !! (x-1)) checkSignals
    --[part 1]--
        --print checkSignals
        --print signalValues
        print $ sum $ zipWith (*) checkSignals signalValues
    --[part 2]--
        --print parseCmd
        let crtDrawing = chunksOf 40 [ let x = fst (cycleStates !! cursor) in 
                            if (cursor `mod` 40) `elem` [x-1..x+1]
                            then '#'
                            else '.'
                                | cursor <- [0..length cycleStates-2]]
        mapM_ print crtDrawing

-- X : shows where the MIDDLE OF THE SPRITE is at 
-- cursor: one by one prints out a symbol every cycle.

-- i wrote it such way cuz i was worried that part 2 will have instructions that takes more cycles
-- not the optimal solutions but... eh
syncCycle :: (RegisterCount,Int) -> String -> [(RegisterCount,Int)]
syncCycle (regis, countPerInstr) cmd
        | cmd == "noop" = [(regis,0)]-- takes 1 cylce, does nothing
        | Just regVal <-  "addx " `stripPrefix` cmd = -- finds value, takes two cycles
                let value = read regVal ::Int
                    newRegis = regis+value
                    notYetDone = (regis, countPerInstr+1)
                    in if countPerInstr == 1 --`debug` ("value:" ++ show value ++ "  X:" ++ show newRegis) -- on second
                            then [(newRegis,  0)] -- update Register, reset
                            else notYetDone : syncCycle notYetDone cmd
 
    -- | otherwise =  error "fucked up" -- fuck you incomplete pattern matching warning
