import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)
import Data.List

isPossible :: String -> Bool
isPossible input = (input == sorted) && (hasRepeated sorted)
    where sorted = sort input

hasRepeated :: String  -> Bool
hasRepeated input =  not (null (filter (\(x,y) -> x == y) (zip (tail input) input)))


part1Tests = TestList[
    TestCase (assertEqual "check repeated" True (hasRepeated ("1223"))),
    TestCase (assertEqual "check not repeated" False (hasRepeated ("123"))),
    TestCase (assertEqual "Check Sample 1" True (isPossible ("111111"))),
    TestCase (assertEqual "Check Sample 2" False (isPossible ("223450"))),
    TestCase (assertEqual "Check Sample 3" False (isPossible ("123789")))
    ]

part1 = length (filter isPossible (map show [264793..803935]))

isPossible2 :: String -> Bool
isPossible2 input = (input == sorted) && (hasRepeated2 sorted)
    where sorted = sort input


isTwoOnly :: (Eq a) => [a] -> Bool
isTwoOnly (a:b:c:d:_) = b == c && a /= b && d /= b 
    

windows :: Int-> [a] -> [[a]]
windows size input = filter (\x -> length x==size) (map (take size) (tails input))

hasRepeated2 :: String  -> Bool
hasRepeated2 input =  (any isTwoOnly (windows 4 input)) || atStart || atEnd
    where
        (a:b:c:_) = input
        (z:y:x:_) = reverse input
        atStart = a == b && a /= c
        atEnd = z == y && z /= x

part2Tests = TestList[
    TestCase (assertEqual "check repeated" True (hasRepeated2 ("112233"))),
    TestCase (assertEqual "check not repeated" False (hasRepeated2 ("123444"))),
    TestCase (assertEqual "Repeated 2 simple" True (hasRepeated2 "1223")),
    TestCase (assertEqual "Repeated 3 simple" False (hasRepeated2 "12223")),    
    TestCase (assertEqual "Repeated 2 start" True (hasRepeated2 "2234")),   
    TestCase (assertEqual "Repeated 2 end" True (hasRepeated2 "21233")),   
    TestCase (assertEqual "Repeated 3 start" False (hasRepeated2 "222123")),   
    TestCase (assertEqual "Repeated 3 end" False (hasRepeated2 "212333")),   
    TestCase (assertEqual "Check Sample 1" True (isPossible2 ("112233"))),
    TestCase (assertEqual "Check Sample 2" False (isPossible2 ("123444"))),
    TestCase (assertEqual "Check Sample 3" True (isPossible2 ("111122")))
    ]

part2 = length (filter isPossible2 (map show [264793..803935]))