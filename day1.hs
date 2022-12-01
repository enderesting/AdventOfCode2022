import System.Environment (getArgs)
import Data.List (sort)
import Data.List.Split (splitWhen)

main :: IO()
main = do
        args <- getArgs -- text file name included in args
        content <- readFile $ head args
        let calorieList = map calorieSum $ splitWhen null $ lines content
        print $ maximum calorieList
        print $ topThree calorieList
        return ()

-- find calorie sum for one elf
calorieSum :: [String] -> Int
calorieSum = sum.map read

-- find calory total for top 3 elves with most calories
topThree :: [Int] -> Int
topThree xs = sum.take 3.reverse $ sort xs
