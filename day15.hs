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

nextStep :: (Integer, MachineState) -> ([Integer], [(Integer, MachineState)])
nextStep (dir, state) = (output newState, children)
    where
        newState = process (state {output = [], isYield = False})
        dirs = if ((output newState) == [0]) then [] else (delete (opposite dir) [1..4] )
        children = map (\i -> (i, newState {input=[i]})) dirs
isNotDone :: [[Integer]] -> Bool
isNotDone l = all (\i -> i /= 2) (concat l)

part1 = length (takeWhile isNotDone (levels tree))
    where 
        tree = unfoldTree nextStep (-1, initialState)
