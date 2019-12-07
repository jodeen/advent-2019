import Data.List
import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)
import IntCode
import DaySevenData

type Transition = MachineState -> MachineState
newState :: Memory -> Int -> MachineState ->  MachineState
newState memory phase (MachineState _ _ _ (output:_) _)  = (MachineState 0 memory [phase, output] [] False)


sample1Data :: [Int]
sample1Data = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
sample1Phases :: [Int]
sample1Phases = [4,3,2,1,0]

transitions :: [Int] -> Memory -> [Transition]
transitions phases memory = map (newState memory) phases

initialState1 = (MachineState 0 [] [] [0] False)

doSteps :: MachineState -> [Transition] -> MachineState
doSteps start [] = start
doSteps start (trans: rest) = doSteps (process (trans start)) rest

doProcess :: [Int] -> Memory -> Int
doProcess phases memory = head (getOutput (doSteps initialState1 (transitions phases memory)))

findMax :: Memory -> Int
findMax memory = maximum (map (\phases -> doProcess phases memory) (permutations [0..4]))

part1Tests = TestList[
    TestCase (assertEqual "sample1_1" 43210 (doProcess sample1Phases sample1Data)),
    TestCase (assertEqual "sample1_2" 54321 (doProcess [0,1,2,3,4] [3,23,3,24,1002,24,10,24,1002,23,-1,23,
        101,5,23,23,1,24,23,23,4,23,99,0,0] )), 
    TestCase (assertEqual "sample1_3" 65210 (doProcess [1,0,4,3,2] [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
        1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] ))
    ]   

part1 = findMax day7Data

blank = (MachineState 0 [] [] [] False)

sample2_1 = doSteps initialState1 (transitions [9,8,7,6,5] [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
    27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])

restart :: MachineState -> [Int] -> MachineState
restart state newInput = process (state {input = (input state) ++ newInput, isYield = False})


initialStates :: [Int] -> Memory -> [MachineState]
initialStates phases memory = (f {input = (input f) ++ [0] }) : r
    where (f: r) = map (\p -> blank {mem = memory, input = [p]}) phases

part2Tests = TestList [
    TestCase (assertEqual "restart" (MachineState 3 [10, 3, 0, 99] [] [] False) (restart (MachineState 1 [0, 3, 0, 99] [] [] True) [10]))
    ]
