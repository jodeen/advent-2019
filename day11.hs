
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import DayElevenData
import IntCode

type Point = (Int, Int)
data Direction = U | D | L | R deriving (Show, Eq)
type Robot = (Point, Direction)
type HullState = Map.Map Point Integer

turn :: Int -> Robot -> Robot
turn 0 ((x,y), U) = ((x-1, y), L)
turn 0 ((x,y), L) = ((x, y-1), D)
turn 0 ((x,y), D) = ((x+1, y), R)
turn 0 ((x,y), R) = ((x, y+1), U)
turn 1 ((x,y), U) = ((x+1, y), R)
turn 1 ((x,y), R) = ((x, y-1), D)
turn 1 ((x,y), D) = ((x-1, y), L)
turn 1 ((x,y), L) = ((x, y+1), U)


startR = ((0,0), U) :: Robot

sample1Data = [(1,0), (0, 0), (1,0), (1,0),(0,1),(1,0), (1,0)] :: [(Int, Int)]

doStepList :: Robot -> [(Int, Int)] -> [Robot]
doStepList start d = scanl (\robot (_, dir) -> turn dir robot) start d

doStep :: (Robot, HullState, MachineState) -> (Robot, HullState, MachineState) 
doStep (robot, hull, state) = (newRobot, newHull, newState)
    where 
        point = fst robot
        color = case Map.lookup point hull of 
            Just(value) -> value 
            Nothing -> 0
        newState = process (state {input=[color], output=[], isYield=False})
        (newColor: dir:_) = output newState
        newHull = Map.insert point newColor hull
        newRobot = turn (fromInteger dir) robot


part1 = length s
    where 
        res = takeWhile (\(_, _, x) -> not (finished x)) (iterate doStep (((0,0), U), Map.empty, (stateWithMem part1Data) {input=[0]}))
        (_, hull, _) = last res
        s = Set.fromList (Map.keys hull)



countUnique :: [Robot] -> Int
countUnique steps = Set.size (Set.fromList (map fst (init steps)))


part2 = addCR 201 (paint canvas (whitePoints hull))
    where 
        res = takeWhile (\(_, _, x) -> not (finished x)) (iterate doStep (((0,0), U), Map.fromList [((0,0), 1)], (stateWithMem part1Data) {input=[1]}))
        (_, hull, _) = last res

part2Hull = hull
    where 
        res = takeWhile (\(_, _, x) -> not (finished x)) (iterate doStep (((0,0), U), Map.empty, (stateWithMem part1Data) {input=[1]}))
        (_, hull, _) = last res        

whitePoints :: Map.Map Point Integer -> Set.Set Point
whitePoints m = Set.fromList (map fst (filter (\(x, i) -> i == 1) (Map.toList m)))

dims :: [Point] -> ((Int, Int), (Int, Int))
dims points =((minimum (map fst points), minimum (map snd points)),  (maximum (map fst points), maximum (map snd points) ))

canvas :: [(Point, Char)]
canvas = map (\p -> (p, ' ')) (cartProd [-100..100] [-100..100]) 

   

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | y <- ys, x <- xs]        

paint :: [(Point, Char)] -> Set.Set Point -> String
paint points white = map (\(p,c) -> if (Set.member p white) then '█' else ' ' ) points

addCR :: Int -> String -> String
addCR _ [] = []
addCR split input = (addCR split t) ++ "\r" ++ h
    where (h,t) = splitAt split input

