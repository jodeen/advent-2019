import IntCode
import qualified Data.Map.Strict as Map
import Data.List
import DayThirteenData

type Tile = ((Integer, Integer), Integer)
type Point = (Integer, Integer)


parseToTile :: [Integer] -> [Tile]
parseToTile [] = []
parseToTile output = ((a,b), c) : (parseToTile rest)
    where
        (a:b:c:rest) = output

isBlock :: Tile -> Bool
isBlock (_,2) = True
isBlock _ = False

blockToChar :: Maybe Integer -> Char
blockToChar a = case a of 
    Just 0 -> ' '
    Just 1 -> '|'
    Just 2 -> '█'
    Just 3 -> '_'
    Just 4 -> '*'
    Nothing -> ' '


part1Output = output (process (stateWithMem part1Data))

part1 = length (filter isBlock (parseToTile part1Output))

blankScreen = [(x,y) | y <- [0..35], x<-[0..35]] :: [Point]

paint :: [Point] -> Map.Map Point Integer -> String
paint screen points = map (\point -> blockToChar (Map.lookup point points))  screen

addCR :: Int -> String -> String
addCR _ [] = []
addCR split input = (addCR split t) ++ "\n" ++ h
    where (h,t) = splitAt split input

findBall :: [Tile] -> Point
findBall tiles = fst (head (filter (\(_,a) -> a == 4) tiles))

findPaddle :: [Tile] -> Point
findPaddle tiles = fst (head (filter (\(_,a) -> a == 3) tiles))

drawScreen :: [Integer] -> String
drawScreen output = addCR 36 (paint blankScreen points)
    where
        points = Map.fromList (parseToTile output)

determineMove :: [Integer] -> Integer
determineMove output 
        | ballX > paddleX = 1
        | ballX < paddleX = -1
        | otherwise = 0
    where 
        tiles = parseToTile output
        (ballX, _) = findBall tiles
        (paddleX, _) = findPaddle tiles

doStep :: MachineState -> MachineState 
doStep state = process (state {input=newInput, output=[]})
    where
        newInput = [determineMove (output state)]

part2 = process ((stateWithMem (2 : tail part1Data)) {input=[1..]})
screen = drawScreen (output part2)

-- part2L = takeWhile (not . finished) (iterate doStep (process ((stateWithMem (2 : tail part1Data)) {input=[]})))
-- screen2 = drawScreen (output (first part2L))