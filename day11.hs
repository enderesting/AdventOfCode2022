{-
I'm so eepy

thought process:
A LIST of monkeys to iterate through <-- it is possible to iterate through a map. using guess what. map!!
                                         check traversal in Data.Map
A MAP of (monkey,monkeyBrain) to keep track of what's going on in there
            -> data MonkeyBrain = (listOfItem, operation, test)

    - spent far too long on the second half ^^; bit worn out really
    - dont be silly about debugging! you can't debug a thing while defining it. silly
    - think about iteration method more deeply before delving into it. I got stuck trying to figure out
        how to link between turns and rounds!
    - be mindful of accumulators! most of the bugs today was me not using the correct map
    - also, i really need to stop making my parser pretty lmao drop those character and go
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE InstanceSigs #-}
import Debug.Trace (trace)
import Data.List (stripPrefix,foldl',sort)
import Data.List.Split (chunksOf,splitOn,endBy)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.Map as M (Map,insertWith,insert,lookup,empty,elems)

debug :: c -> String -> c
debug = flip trace

main :: IO()
main = do
        -- args <- getArgs -- text file name included in args
        content <- readFile "day11.txt"
        let monkeyStr = endBy "\n\n" content
        let monkeys = map (fromJust.makeMonkey) monkeyStr
        let monkeyMap = foldl' (\acc x -> insert (name x) x acc) empty monkeys
        let monkeyRound x = foldl' monkeyTurn x [0.. (length monkeys-1)]
        let monkeyGame n =  take n $ iterate monkeyRound monkeyMap

        let lastGame = M.elems $ last (monkeyGame 10001)
        let freq = map counter lastGame
        print lastGame
        print $ product $ take 2 $ reverse $ sort freq


monkeyTurn :: M.Map Monkey MonkeyBrain -> Monkey -> M.Map Monkey MonkeyBrain
monkeyTurn mkMap mkInTurn
    = insert name (Monkey name [] operation test (counter+ length items)) afterThrown --`debug` ("afterthrown: " ++ show afterThrown)
    -- part 1 --
    --   where inspectedItems = map ((`div` 3).operation ) items --`debug` ("check:" ++ show inspectedItems)
    -- part 2 --                         this? the product of all prime number tests
      where inspectedItems = map ((`mod` 9699690).operation) items --`debug` ("check:" ++ show inspectedItems)
            -- findBrain mkName = updateItem $ fromJust $ M.lookup mkName mkMap  -- <-- the reason this didnt work is because i need the acc
            mkInPlay@(Monkey name items operation test counter) = fromJust $ M.lookup mkInTurn mkMap
            afterThrown = foldl' (\acc x -> let toThisMk = test x
                                                findBrain mkName mkMap newItem = updateMonkey (fromJust $ M.lookup mkName mkMap) newItem
                                                in -- x = which monkey index it goes to
                            M.insert toThisMk -- insert this monkey
                            (findBrain toThisMk acc x) --'s brain to
                            acc) --this map
                            --`debug` ("throw to this monkey: " ++ show toThisMk)
                            mkMap inspectedItems -- `debug` ("monkey " ++ show mkInTurn ++ " thrown " ++ show inspectedItems)


updateMonkey :: MonkeyBrain -> Item -> MonkeyBrain
updateMonkey mk@(Monkey name items operation test counter) item
    = Monkey {name = name , listOfItems = items++[item], operation = operation, test = test, counter = counter}

type Item = Int -- also the worry level
type Monkey = Int
data MonkeyBrain = Monkey {name :: Monkey, listOfItems :: [Item], operation :: Item->Item, test :: Item->Monkey, counter::Int}

instance Show MonkeyBrain where
    show :: MonkeyBrain -> String
    show mkBrain@(Monkey name items operation test counter)
        = "monkey " ++ show name ++ "; counter:" ++ show counter ++ show items  ++ "\n" -- ++ "  f(x) = " ++ show (operation 1) ++ 

makeMonkey :: [Char] -> Maybe MonkeyBrain
makeMonkey mkyStr = do
                    let mkyData = lines mkyStr
                    nameStr <- "Monkey " `stripPrefix` init (head mkyData)
                    let name = read nameStr :: Int
                    itemsStr <- "  Starting items: " `stripPrefix` (mkyData !! 1)
                    let items = read ("["++itemsStr++"]") :: [Int]
                    operationStr <- "  Operation: new = " `stripPrefix` (mkyData !! 2)
                    let operation = readOperation operationStr
                    divisor <- "  Test: divisible by " `stripPrefix` (mkyData !! 3)
                    trueMky <- "    If true: throw to monkey " `stripPrefix` (mkyData !! 4)
                    falseMky <- "    If false: throw to monkey " `stripPrefix` (mkyData !! 5)
                    let test x = if x `mod` (read divisor :: Int) == 0
                                    then (read trueMky :: Int)
                                    else (read falseMky :: Int)
                    let mk = Monkey {name = name, listOfItems = items, operation = operation, test = test, counter = 0}
                    return mk

readOperation :: String -> (Item->Item)
readOperation xs = \x -> parseOperator oper (parseOp (opList !! 0) x) (parseOp (opList !! 2) x)
            where opList = splitOn " " xs
                  parseOperator x
                            | x == "+" = (+)
                            | x == "*" = (*)
                            | otherwise = error "dont support"
                  parseOp op x
                            | op == "old" = x
                            | otherwise = read op ::Int
                  op1 = opList !! 0
                  op2 = opList !! 2
                  oper = opList !! 1
