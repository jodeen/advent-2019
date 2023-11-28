import DayOneData(part1Data)
import Test.HUnit

calcFuel :: Integer -> Integer
calcFuel mass = mass `div` 3 - 2

calcModules :: [Integer] -> Integer
calcModules modules = sum (map calcFuel modules)

test1_1 = TestCase (assertEqual "Should be 2" 2 (calcFuel 12))
test1_2 = TestCase (assertEqual "Should be 2" 2 (calcFuel 14))
test1_3 = TestCase (assertEqual "Should be 654" 654 (calcFuel 1969))
test1_4 = TestCase (assertEqual "Should be 33583" 33583 (calcFuel 100756))

part1Tests = TestList[test1_1, test1_2, test1_3, test1_4]

part1 = calcModules part1Data

calcFullFuel:: Integer -> Integer
calcFullFuel mass
    | newFuel <= 0  = 0
    | otherwise = newFuel + (calcFullFuel newFuel)
    where newFuel = calcFuel mass

calcFullModules :: [Integer] -> Integer
calcFullModules modules = sum (map calcFullFuel modules)

test2_1 = TestCase (assertEqual "Should be 2" 2 (calcFullFuel 12))
test2_2 = TestCase (assertEqual "Should be 966" 966 (calcFullFuel 1969))
test2_3 = TestCase (assertEqual "Should be 50346" 50346 (calcFullFuel 100756))

part2 = calcFullModules part1Data



part2Tests = TestList[test2_1, test2_2, test2_3]

