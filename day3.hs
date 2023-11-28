import Data.List
import qualified Data.Map.Strict as Map
import Test.HUnit  hiding (filter, foo, map)
import DayThreeData
import Data.Maybe

type Point = (Int, Int)
type Segment = (Point, Point)

origin = ((0,0), (0,0))


doStep :: Point -> Instruction -> Point
doStep (x,y) ('U', dist) = (x, y-dist)
doStep (x,y) ('D', dist) = (x, y+dist)
doStep (x,y) ('R', dist) = (x+dist, y)
doStep (x,y) ('L', dist) = (x-dist, y)

createSegment :: Segment -> Instruction -> Segment
createSegment (_, end) inst = (end, doStep end inst)

generatePath :: Segment -> [Instruction] -> [Segment]
generatePath start instructions = scanl createSegment start instructions


isVertical :: Segment -> Bool
isVertical ((startX, _), (endX, _)) = startX == endX

isHorizontal :: Segment -> Bool
isHorizontal ((_, startY) , (_, endY)) = startY == endY

areParallelVertical :: Segment -> Segment -> Bool
areParallelVertical a b = (isVertical a) && (isVertical b)

areParallelHorizontal :: Segment -> Segment -> Bool
areParallelHorizontal a b = (isHorizontal a) && (isHorizontal b)

verticalSort :: Segment -> Segment
verticalSort (a, b) 
    | (snd a) > (snd b) = (b,a)
    | otherwise = (a,b)

horizontalSort :: Segment -> Segment
horizontalSort (a, b) 
    | (fst a) > (fst b) = (b,a)
    | otherwise = (a,b)    
     
verticalOverlap :: Segment -> Segment -> [Point]
verticalOverlap ((aStartX, aStartY), (aEndX, aEndY)) ((bStartX, bStartY), (bEndX, bEndY)) 
    | (aStartX == bStartX) && ((between bStartY aStartY aEndY) || (between bEndY aStartY aEndY) 
        || (between aStartY bStartY bEndY) || (between aEndY bStartY bEndY)) = map (\y -> (aStartX, y)) [(max aStartY  bStartY)..(min aEndY bEndY)]
    | otherwise = []

horizontalOverlap :: Segment -> Segment -> [Point]
horizontalOverlap ((aStartX, aStartY), (aEndX, aEndY)) ((bStartX, bStartY), (bEndX, bEndY)) 
    | (aStartY == bStartY) && ((between bStartX aStartX aEndX) || (between bEndX aStartX aEndX) 
        || (between aStartX bStartX bEndX) || (between aEndX bStartX bEndX)) = map (\x -> (x, aStartY)) [(max aStartX  bStartX)..(min aEndX bEndX)]
    | otherwise = []

crossOverlap :: Segment -> Segment -> [Point]
crossOverlap a b 
    | (between horStartY vertStartY vertEndY) && (between vertStartX horStartX horEndX) = [(vertStartX, horStartY)]
    | otherwise = []
    where 
        ((horStartX, horStartY), (horEndX, horEndY)) = horizontalSort (if isHorizontal a 
                                then a
                                else b)
        ((vertStartX, vertStartY), (vertEndX, vertEndY)) = verticalSort (if isVertical a 
                                then a
                                else b)
    

calcOverlap :: (Segment, Segment) -> [Point]
calcOverlap (a, b)
    | areParallelVertical a b = verticalOverlap (verticalSort a) (verticalSort b)
    | areParallelHorizontal a b = horizontalOverlap (horizontalSort a) (horizontalSort b)
    | otherwise = crossOverlap a b

between :: Int -> Int -> Int -> Bool
between point start end = point >= start && point <= end

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]


crosses :: [Instruction] -> [Instruction] -> [Point]
crosses a b = concat (map calcOverlap allCombos)
    where
        aPath = generatePath origin a
        bPath = generatePath origin b
        allCombos = cartProd aPath bPath

dist :: Point -> Int
dist (a,b) = (abs a) + (abs b)

minDist :: [Instruction] -> [Instruction] -> Int
minDist a b = minimum (filter (\x -> x /= 0) (map dist (crosses a b)))



test1_1 = TestCase (assertEqual "simple up Step" (0, -1) (doStep (0,0) ('U', 1)))
test1_2 = TestCase (assertEqual "parse inst" ('U', 100) (parseInstruction "U100"))
test1_3 = TestCase (assertEqual "segment" ((0,0), (10, 0)) (createSegment ((0,0), (0,0)) ('R', 10)))
test1_4 = TestCase (assertEqual "vert parallel" True (areParallelVertical ((0,0), (0,10)) ((50, 50), (50, 60))))
test1_5 = TestCase (assertEqual "vert parallel" True (areParallelVertical ((0,0), (0,10)) ((0,10), (0,10))))
test1_6 = TestCase (assertEqual "vert not parallel" False (areParallelVertical ((10,0), (0,0)) ((0,10), (0,10))))
test1_7 = TestCase (assertEqual "vert no overlap different x" [] (verticalOverlap ((0,0), (0,10)) ((5,10), (5,10))))
test1_8 = TestCase (assertEqual "vert no overlap same x" [] (verticalOverlap ((0,0), (0,10)) ((0,11), (0,20))))
test1_9 = TestCase (assertEqual "vert overlap fully" [(0,0), (0,1), (0,2)] (verticalOverlap ((0,0), (0,2)) ((0,0), (0,2))))
test1_10 = TestCase (assertEqual "vert overlap partially first above" [(0,1), (0,2)] (verticalOverlap ((0,0), (0,2)) ((0,1), (0,3))))
test1_11 = TestCase (assertEqual "vert overlap partially first below" [(0,0), (0,1)] (verticalOverlap ((0,0), (0,2)) ((0,-2), (0,1))))
test1_12 = TestCase (assertEqual "calc vert overlap" [(0,0), (0,1)] (calcOverlap (((0,0), (0,2)), ((0,1), (0,-2)))))
test1_13 = TestCase (assertEqual "calc cross overlap" [(0,0)] (crossOverlap ((-5,0), (5,0)) ((0,5), (0,-5))))
test1_14 = TestCase (assertEqual "calc cross no overlap" [] (crossOverlap ((-5,0), (5,0)) ((6,5), (6,-5))))
test1_15 = TestCase (assertEqual "calc cross overlap" [(0,1)] (crossOverlap ((-5,1), (5,1)) ((0,5), (0,-5))))

test1_16 = TestCase (assertEqual "sample 1" 6 (minDist sample1A sample1B))
test1_17 = TestCase (assertEqual "sample 2" 159 (minDist sample2A sample2B))

part1Tests = TestList[test1_1, test1_2, test1_3, test1_4, test1_5, test1_6, test1_7, test1_8, test1_9, test1_10, 
    test1_11, test1_12, test1_13, test1_14, test1_15, test1_16, test1_17]

sample1A = map parseInstruction ["R8","U5","L5","D3"]
sample1B = map parseInstruction ["U7","R6","D4","L4"]

sample2A = map parseInstruction ["R75","D30","R83","U83","L12","D49","R71","U7","L72"]
sample2B = map parseInstruction ["U62","R66","U55","R34","D71","R55","D58","R83"]

part1 = minDist dataA dataB


expand :: Point -> Instruction -> [Point]
expand (x,y) ('U', dist) = zip (replicate dist x) (reverse [y-dist..y-1])
expand (x,y) ('D', dist) = zip (replicate dist x) [y+1..y+dist]
expand (x,y) ('R', dist) = zip [x+1..x+dist] (replicate dist y)
expand (x,y) ('L', dist) = zip (reverse [x-dist..x-1]) (replicate dist y)

expandList :: [Instruction] -> [Point]
expandList instructions = concat (scanl (\points inst -> expand (last points) inst) [(0,0)] instructions)

test2_1 = TestCase (assertEqual "simple up Step" [(0,-1), (0,-2), (0,-3)] (expand (0,0) ('U', 3)))
test2_2 = TestCase (assertEqual "simple down Step" [(0,1), (0,2), (0,3)] (expand (0,0) ('D', 3)))
test2_3 = TestCase (assertEqual "simple right Step" [(1,0), (2,0), (3,0)] (expand (0,0) ('R', 3)))
test2_4 = TestCase (assertEqual "simple left Step" [(-1,0), (-2,0), (-3,0)] (expand (0,0) ('L', 3)))

test2_5 = TestCase (assertEqual "sample 1" (Just 30) (findTimed sample1A sample1B))
test2_6 = TestCase (assertEqual "sample 2" (Just 610) (findTimed sample2A sample2B))

part2Tests = TestList[test2_1, test2_2, test2_3, test2_4, test2_5, test2_6]

findTimed :: [Instruction] -> [Instruction] -> Maybe Int
findTimed a b = minimum (map snd allCollisions)
        where 
            aMap = Map.fromList(reverse (zip (tail (expandList a)) [1..]))
            bList = zip (tail (expandList b)) [1..]
            joined = map (\(point, cost) -> (point, fmap (+cost) (Map.lookup point aMap) )) bList
            
            allCollisions = filter (\(p,c) -> isJust c) joined

part2 = findTimed dataA dataB