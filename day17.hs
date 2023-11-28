import Data.List 
import qualified Data.Set as Set
import DayData
import IntCode
import Data.Char

type Point = (Int, Int)
type Cell = (Point, Char)

findNonCorners :: [[Cell]] -> [Point]
findNonCorners rows = concat (map findNonCornerRow rows)
    
        
findNonCornerRow :: [Cell] -> [Point]
findNonCornerRow row = map (\(_,(p,_), _) -> p) (filter isMiddle triples)
    where
        triples = zip3 (((0,0), '.'):row) row (tail row)

isMiddle :: (Cell, Cell, Cell) -> Bool
isMiddle ((_, a),(_, b),(_,c)) = a == '#' && b == '#' && c =='#'


fillInX :: ([Char], Int) -> [Cell]
fillInX (chars, y) = map (\(c, x) -> ((x,y), c)) (zip chars [0..])

toCells :: [[Char]] -> [[Cell]]
toCells cs = map fillInX withY
    where
        withY = zip cs [0..]

sample1Data = toCells
    [
        "..#..........",
        "..#..........",
        "#######...###",
        "#.#...#...#.#",
        "#############",
        "..#...#...#..",
        "..#####...^.."
    ]

calcIntersections :: [[Cell]] -> [Point]
calcIntersections cells = Set.toList (Set.intersection rows cols)
    where
        rows = Set.fromList (findNonCorners cells)
        cols = Set.fromList (findNonCorners (transpose cells))

calcAlignment :: [Point] -> Int
calcAlignment points = sum (map (\(a,b) -> a*b) points)



part1 = calcAlignment (calcIntersections (toCells strs))
    where 
        out = output (process (stateWithMem day17Data))
        outChar = map (\c -> chr (fromIntegral  c)) out
        strs = lines outChar