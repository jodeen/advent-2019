import DayData
import IntCode
import Data.Tree
import Data.List


initialState = (stateWithMem day15Data) {output=[-1]}

opposite :: Integer -> Integer
opposite 1 = 2
opposite 2 = 1
opposite 3 = 4
opposite 4 = 3
opposite x = x

nextStep :: (Integer, MachineState) -> (MachineState, [(Integer, MachineState)])
nextStep (dir, state) = (newState, children)
    where
        newState = process (state {output = [], isYield = False})
        dirs = if ((output newState) == [0]) then [] else (delete (opposite dir) [1..4] )
        children = map (\i -> (i, newState {input=[i]})) dirs
isNotDone :: [MachineState] -> Bool
isNotDone states = all (\i -> i /= 2) (concat (map output states))

part1 = length (takeWhile isNotDone (levels tree))
    where 
        tree = unfoldTree nextStep (-1, initialState)

findDone :: [MachineState] -> MachineState
findDone states = head (filter (\s -> (output s) == [2]) states)

-- the 2 is because 1 for the root and one for the leaves
part2 = length (levels o2Spread) - 2
    where 
        tree = unfoldTree nextStep (-1, initialState)
        levs = head (dropWhile isNotDone (levels tree))
        o2State = findDone levs
        o2Spread = unfoldTree nextStep (-1, o2State)
        