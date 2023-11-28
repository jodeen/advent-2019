import Data.List
import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)
import DayEightData

type Layer = [Int]

breakAt :: Int -> [a] -> [[a]]
breakAt idx [] = []
breakAt idx list = (take idx list) : (breakAt idx (drop idx list))


createLayers :: Int -> Int -> [Int] -> [Layer]
createLayers x y input = breakAt (x*y) input

countItem :: Int -> Layer -> Int
countItem item layer = length (filter (==item) layer)

part1CalcLayer :: Layer -> Int
part1CalcLayer layer = (length (filter (==1) layer)) * (length (filter (==2) layer))

part1Calc :: [Layer] -> [(Int, Int)]
part1Calc layers = zip (map part1CalcLayer layers) [1..]

part1Count :: Int -> Int -> [Int] -> Int
part1Count x y input = part1CalcLayer minLayer
    where        
        layers = createLayers x y input
        zeroCounts = map (\layer -> (countItem 0 layer, layer)) layers
        (_, minLayer) = minimumBy (\(a,_) (b,_) -> compare a b) zeroCounts
        
        
part1Tests = TestList [
    TestCase (assertEqual "parseInput" [1,2,3] (parseInput "123")),    
    TestCase (assertEqual "break" [[1,2,3],[4,5,6], [7]] (breakAt 3 [1..7])),
    TestCase (assertEqual "count item" 4 (countItem 3 [3,4,5,3,4,3,3,4])),
    TestCase (assertEqual "calc layer" 1 (part1CalcLayer [1,2,3,4])),
    TestCase (assertEqual "calc layer" 2 (part1CalcLayer [1,2,2,4])),
    TestCase (assertEqual "calc layer" 2 (part1CalcLayer [1,1,2,4])),
    TestCase (assertEqual "calc layer" 4 (part1CalcLayer [1,2,2,1])),
    TestCase (assertEqual "sample 1" 1 (part1Count 3 2 [1,2,3,4,5,6,7,8,1,0,1,2]))
    ]

part1 = part1Count 25 6 part1Data

visible :: (Int, Int) -> Int
visible (0, _) = 0
visible (1, _) = 1
visible (2, a) = a


combine :: Layer -> Layer -> Layer
combine current new = map visible (zip current new) 

toPixel :: Int -> String
toPixel 1 = "█"
toPixel 0 = " "
toPixel a = show a

displayLayer :: Int -> Layer -> String 
displayLayer width layer = intercalate "\n" rows
    where 
        layerString = intercalate "" (map toPixel layer)
        rows = breakAt width layerString

calcFinal :: [Layer] -> Layer
calcFinal (initial: rest) = foldl combine initial rest


part2Run :: Int -> Int -> [Int] -> String
part2Run x y input = (displayLayer x final) ++ "\n"
    where
        layers = createLayers x y input
        final = calcFinal layers

part2Tests = TestList [
    TestCase (assertEqual "parseInput" [1,2,3] (parseInput "123"))
    ]


part2 = part2Run 25 6 part1Data

-- output results with putStr part2
