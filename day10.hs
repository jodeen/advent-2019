import DayTenData
import Data.Maybe 
import Data.List 
import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)
import qualified Data.Set as Set


type Point = (Int, Int)
type Asteroid = Maybe Char

distance :: Point -> Point -> Int
distance (ax, ay) (bx, by) = round (sqrt (fromIntegral ((bx-ax)*(bx-ax) + (by-ay)*(by-ay))))
    

parseChar :: Char -> Asteroid
parseChar '.' = Nothing
parseChar '#' = Just '#'

parseLine :: [Char]-> [Asteroid]
parseLine chars = map parseChar chars

parseInput :: [String] -> [[Asteroid]]
parseInput lines = map parseLine lines

fillInX :: ([Asteroid], Int) -> [(Point, Asteroid)]
fillInX (asteroids, y) = map (\(asteroid, x) -> ((x,y), asteroid)) (zip asteroids [0..])

toPoints :: [[Asteroid]] -> [Point]
toPoints asteroids = map fst pointsOnly
    where
        withY = zip asteroids [0..]
        withX = map fillInX withY
        pointsOnly = filter (\(p, asteroid) -> isJust asteroid) (concat withX) 

dimensions :: [[Asteroid]] -> (Int, Int)
dimensions asteroids = (length (asteroids !! 0), length asteroids)

-- generateHidden :: Point -> Point -> [Point]
-- generateHidden (startX, startY) (endX, endY) =  

slope :: Point -> Point -> Rational
slope (startX, startY) (endX, endY) = (fromIntegral (endY - startY)) / (fromIntegral (endX - startX))

lineEqn :: Point -> Point -> (Int -> (Int, Rational))
lineEqn (startX, startY) (endX, endY) = (\x -> (x,  m * (fromIntegral x) + b))
    where
        m = slope (startX, startY) (endX, endY)
        b = (fromIntegral startY) - m * (fromIntegral startX)
   
getExactPoints :: [(Int, Rational)] -> [Point]
getExactPoints p = map (\(a,b) -> (a, round b)) exact
    where
        exact = filter (\x -> isInt (snd x)) p

rev :: (Int, Int) -> (Int, Int) 
rev (a,b) = (b,a)

occluded :: (Int, Int) -> Point -> Point -> [Point]
occluded (maxX, maxY) start end = xPoints
    where
        (startX, startY) = start
        (endX, endY) = end
        xPoints = getExactPoints (map (lineEqn start end)  [startX .. maxX])

isInt :: Rational -> Bool
isInt r = fromInteger (round r) == r

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

isVisible :: Set.Set Point -> Point -> Point -> Bool
isVisible pointSet start end = null (filter (\x -> Set.member x pointSet) (xPoints ++ yPoints))
    where 
        (startX, startY) = start
        (endX, endY) = end
        xPoints = getExactPoints (map (lineEqn start end)  [(min startX endX)+1 .. (max startX endX)-1])
        yPoints = map rev (getExactPoints (map (lineEqn (rev start) (rev end))  [(min startY endY)+1 .. (max startY endY)-1]))


getVisible :: Set.Set Point -> Point -> Set.Set Point
getVisible pointSet start = Set.filter (isVisible pointSet start) (Set.delete start pointSet)

countVisible :: Set.Set Point -> Set.Set (Point, Int)
countVisible points = Set.map (\x -> (x, length (getVisible points x))) points

doPart1 :: [String] -> (Point, Int)
doPart1 input = maximumBy (\(_,a) (_,b) -> compare a b) (Set.toList visible)
    where 
        points = toPoints (parseInput input)
        visible = countVisible (Set.fromList points)

part1 = doPart1 part1Input

part1Tests = TestList [
    TestCase (assertEqual "dimensions" (5,5) (dimensions (parseInput sample1))),    
    TestCase (assertEqual "toPoints" [(1,0),(4,0),(0,2),(1,2),(2,2),(3,2),(4,2),(4,3),(3,4),(4,4)] (toPoints (parseInput sample1))),
    TestCase (assertEqual "is Visible simple" True (isVisible (Set.fromList [(0,0), (2,2)]) (0,0) (2,2))),
    TestCase (assertEqual "isnot Visible simple" False (isVisible (Set.fromList [(0,0), (1,1), (2,2)]) (0,0) (2,2))),
    TestCase (assertEqual "is Visible simple" True (isVisible (Set.fromList [(0,0), (1,2), (2,2)]) (0,0) (2,2))),
    TestCase (assertEqual "is Visible vertical" True (isVisible (Set.fromList [(0,0), (0,2)]) (0,0) (0,2))), 
    TestCase (assertEqual "is Visible vertical reverse" True (isVisible (Set.fromList [(0,0), (0,2)]) (0,2) (0,0))), 
    TestCase (assertEqual "is not Visible vertical" False (isVisible (Set.fromList [(0,0), (0,1), (0,2)]) (0,0) (0,2))), 
    TestCase (assertEqual "is not Visible vertical reverse" False (isVisible (Set.fromList [(0,0), (0,1), (0,2)]) (0,2) (0,0))), 
    TestCase (assertEqual "is Visible horizontal" True (isVisible (Set.fromList [(0,0), (2,0)]) (0,0) (2,0))), 
    TestCase (assertEqual "is Visible horizontal reverse" True (isVisible (Set.fromList [(0,0), (2,0)]) (2,0) (0,0))), 
    TestCase (assertEqual "is not Visible horizontal" False (isVisible (Set.fromList [(0,0), (1,0), (2,0)]) (0,0) (2,0))),
    TestCase (assertEqual "is not Visible horizontal reverse" False (isVisible (Set.fromList [(0,0), (1,0), (2,0)]) (2,0) (0,0))),
    TestCase (assertEqual "getVisible simple" (Set.fromList [(0,1), (1,0)]) (getVisible (Set.fromList [(0,0), (1,0), (0,1)]) (0,0) )),
    TestCase (assertEqual "getVisible simple" (Set.fromList [(0,1), (1,0)]) (getVisible (Set.fromList [(0,0), (1,0), (0,1), (0,2)]) (0,0) )),
    TestCase (assertEqual "sample 1" ((3,4), 8) (doPart1 sample1)),
    TestCase (assertEqual "sample 2" ((5,8), 33) (doPart1 sample2)),
    TestCase (assertEqual "sample 3" ((1,2), 35) (doPart1 sample3)),
    TestCase (assertEqual "sample 4" ((6,3), 41) (doPart1 sample4))
    -- TestCase (assertEqual "sample 5" ((11,13), 210) (doPart1 sample5))
    ]

angle :: Point -> Point -> Float
angle start end = acos (fromIntegral (fst start - fst end) / fromIntegral (distance start end))


nextShot :: [Point] -> Point -> Point
nextShot points point = minimumBy (\a b -> compare (angle point a) (angle point b)) points
    where
        visible = getVisible (Set.fromList points) point