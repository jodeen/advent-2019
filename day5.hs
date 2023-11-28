import DayFourData(part1Data)
import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)
import Prelude

type Memory = [Int]
type Input = Int
type Output = [Int]
type InstPoint = Int
type MachineState = (InstPoint, Memory, Input, Output)
type OpCode = (Int, [Mode])
data Mode = Pos | Imm

getArgs :: Int -> Int-> Memory -> Memory
getArgs idx count memory = take count (drop idx memory)

add :: OpCode -> MachineState ->  MachineState
add (_, modes) (ip, memory, input, output) = (ip + 4, putMemory resIndex (a + b) memory, input, output)
    where
        (aIndex: bIndex: resIndex: _) = getArgs (ip+1) 3 memory
        a = getMemory (modes !! 0) aIndex memory
        b = getMemory (modes !! 1) bIndex memory

mult :: OpCode -> MachineState ->  MachineState
mult (_, modes) (ip, memory, input, output) = (ip + 4, putMemory resIndex (a * b) memory, input, output) 
    where
        (aIndex: bIndex: resIndex: _) = getArgs (ip+1) 3 memory
        a = getMemory (modes !! 0) aIndex memory
        b = getMemory (modes !! 1) bIndex memory

stor :: OpCode -> MachineState -> MachineState
stor (_, modes) (ip, memory, input, output) = (ip + 2, putMemory resIndex input memory, input, output)
    where (resIndex : _ ) = getArgs (ip + 1) 2 memory    

out :: OpCode -> MachineState -> MachineState
out (_, modes) (ip, memory, input, output) = (ip + 2, memory, input, output ++ [getMemory (modes!!0) lookupIdx memory]) 
    where (lookupIdx : _) = getArgs (ip + 1) 1 memory

getMemory :: Mode -> Int -> Memory -> Int
getMemory Pos idx memory = memory !! idx
getMemory Imm value _ = value

putMemory :: Int -> Int -> Memory -> Memory
putMemory idx value memory = h ++ [value] ++ (tail t) where
    (h, t) = splitAt idx memory

processStep :: MachineState -> MachineState
processStep state
    | opCode == 1 = add op state
    | opCode == 2 = mult op state
    | opCode == 3 = stor op state
    | opCode == 4 = out op state
    | opCode == 5 = jmpt op state
    | opCode == 6 = jmpf op state
    | opCode == 7 = lt op state
    | opCode == 8 = equ op state
    | otherwise = error ("Unexpected opcode " ++ (show opCode) ++ " at position " ++ (show pos))
    where 
        (pos, memory, input, output) = state
        op = parseOp (getMemory Pos pos memory)
        (opCode, _) = op

isComplete :: MachineState -> Bool
isComplete (pos, memory, _, _)= opCode == 99
    where opCode = getMemory Pos pos memory

lpad :: Int -> Int -> String
lpad num len = reverse (take len (reverse ((replicate len '0') ++ (show num))))

numToMode :: Char -> Mode
numToMode '0' = Pos
numToMode '1' = Imm

parseOp :: Int -> OpCode
parseOp op = (read [d,e], [numToMode c,numToMode b, numToMode a])
    where
        (a: b: c: d: e: _) = lpad op 5


process :: MachineState -> MachineState
process state = head (filter isComplete (iterate processStep state))

part1Tests = TestList[
    TestCase (assertEqual "getArgs" [2,3,4] (getArgs 1 3 [1,2,3,4,5,6])),
    TestCase (assertEqual "Simple get" 2 (getMemory Pos 1 [1,2,3,4,5,6])),
    TestCase (assertEqual "Simple add " (4, [2,0,0,0,99], 0, []) (add (1, [Pos, Pos, Pos]) (0, [1,0,0,0,99], 0, []))),
    TestCase (assertEqual "Simple processStep " (4, [2,0,0,0,99], 0, []) (processStep (0, [1,0,0,0,99], 0, []))),
    TestCase (assertEqual "Simple process " (4, [2,0,0,0,99], 0, []) (process (0, [1,0,0,0,99], 0, []))),
    TestCase (assertEqual "process " (8, [30,1,1,4,2,5,6,0,99], 0, []) (process (0, [1,1,1,4,99,5,6,0,99], 0, []))),
    TestCase (assertEqual "simple mult" (4, [2,4,4,5,99,9801], 0, []) (process (0, [2,4,4,5,99,0], 0, []))),
    TestCase (assertEqual "store" (2, [3,2,5], 5, []) (stor (3, [Pos, Pos, Pos]) (0, [3,2,0], 5, []))),
    TestCase (assertEqual "output" (2, [4,2,6], 5, [6]) (out (4, [Pos, Pos, Pos]) (0, [4,2,6], 5, []))),
    TestCase (assertEqual "in to out" (4, [1,0,4,0,99], 1, [1]) (process (0, [3,0,4,0,99], 1, []))),
    TestCase (assertEqual "simple full" (4, [1101,100,-1,4,99], 1, []) (process (0, [1101,100,-1,4,0], 1, [])))
    ]


getOutput :: MachineState -> [Int]
getOutput state = output
    where (_, _, _, output) = process state

(_, _, _, part1) = process (0, part1Data, 1, [])



jmpt :: OpCode -> MachineState -> MachineState
jmpt (_, modes) (ip, memory, input, output) = (if test /= 0 then res else ip + 3, memory, input, output)
    where 
        (a : b : _ ) = getArgs (ip + 1) 3 memory    
        test = getMemory (modes !! 0) a memory
        res = getMemory (modes !! 1) b memory   

jmpf :: OpCode -> MachineState -> MachineState
jmpf (_, modes) (ip, memory, input, output) = (if test == 0 then res else ip + 3, memory, input, output)
    where 
        (a : b : _ ) = getArgs (ip + 1) 3 memory    
        test = getMemory (modes !! 0) a memory
        res = getMemory (modes !! 1) b memory     

lt :: OpCode -> MachineState -> MachineState
lt (_, modes) (ip, memory, input, output) = (ip + 4, newMemory, input, output)
    where 
        (a : b : result : _ ) = getArgs (ip + 1) 4 memory    
        testA = getMemory (modes !! 0) a memory
        testB = getMemory (modes !! 1) b memory    
        newMemory =  putMemory result (if testA < testB then 1 else 0) memory

equ :: OpCode -> MachineState -> MachineState
equ (_, modes) (ip, memory, input, output) = (ip + 4, newMemory, input, output)
    where 
        (a : b : result : _ ) = getArgs (ip + 1) 4 memory    
        testA = getMemory (modes !! 0) a memory
        testB = getMemory (modes !! 1) b memory    
        newMemory =  putMemory result (if testA == testB then 1 else 0) memory        

part2Tests = TestList[
    TestCase (assertEqual "equal to 8 false" [0] (getOutput (0, [3,9,8,9,10,9,4,9,99,-1,8], 7, []))),
    TestCase (assertEqual "equal to 8 true" [1] (getOutput (0, [3,9,8,9,10,9,4,9,99,-1,8], 8, []))),
    TestCase (assertEqual "less than 8 false" [0] (getOutput (0, [3,9,7,9,10,9,4,9,99,-1,8], 8, []))),
    TestCase (assertEqual "less than 8 true" [1] (getOutput (0, [3,9,7,9,10,9,4,9,99,-1,8], 7, []))),
    TestCase (assertEqual "imm eq 8 true" [1] (getOutput (0, [3,3,1108,-1,8,3,4,3,99], 8, []))),
    TestCase (assertEqual "imm eq 8 false" [0] (getOutput (0, [3,3,1108,-1,8,3,4,3,99], 9, []))),
    TestCase (assertEqual "imm lt 8 true" [1] (getOutput (0, [3,3,1107,-1,8,3,4,3,99], 7, []))),
    TestCase (assertEqual "imm lt 8 false" [0] (getOutput (0, [3,3,1107,-1,8,3,4,3,99], 9, []))),
    TestCase (assertEqual "pos jump 1" [1] (getOutput (0, [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], 2, []))),
    TestCase (assertEqual "pos jump 0" [0] (getOutput (0, [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], 0, []))),
    TestCase (assertEqual "im jump 1" [1] (getOutput (0, [3,3,1105,-1,9,1101,0,0,12,4,12,99,1], 9, []))),
    TestCase (assertEqual "im jump 0" [0] (getOutput (0, [3,3,1105,-1,9,1101,0,0,12,4,12,99,1], 0, []))),
    TestCase (assertEqual "larger lt 8" [999] (getOutput (0, [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 6, []))),
    TestCase (assertEqual "larger eq 8" [1000] (getOutput (0, [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 8, []))),
    TestCase (assertEqual "larger gt 8" [1001] (getOutput (0, [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 11, [])))
    
    ]
part2 = getOutput (0, part1Data, 5, [])