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

sampleMemory2_1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
    27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
sample2_1 = doSteps initialState1 (transitions [9,8,7,6,5] sampleMemory2_1)



initialStates :: [Int] -> Memory -> [MachineState]
initialStates phases memory = (f {input = (input f) ++ [0] }) : r
    where (f: r) = map (\p -> blank {mem = memory, input = [p]}) phases

feedback :: [MachineState] -> [MachineState]
feedback (one : two: rest) 
    | (finished newState) && (finished two) = newState : two : rest 
    | otherwise = feedback (newTwo : (rest ++ [newState]))
    where 
        newState = process (one {output = [], isYield = False})
        newTwo = two {input = (input two) ++ output newState}
                
calcThruster :: Memory -> [Int] -> Int
calcThruster memory phases = head (reverse (output (head (feedback (initialStates phases memory)))))

findMax2 :: [Int] -> Memory -> Int
findMax2 phases memory = maximum (map (calcThruster memory) (permutations phases))
    

part2Tests = TestList [
    TestCase (assertEqual "sample 2_1" 139629729 (calcThruster sampleMemory2_1 [9,8,7,6,5])),
    TestCase (assertEqual "sample 2_2" 18216 (calcThruster [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] [9,7,8,5,6])),
    TestCase (assertEqual "sample 2_1" 139629729 (findMax2 [5..9] sampleMemory2_1))
    ]

part2 = findMax2 [5..9] day7Data
