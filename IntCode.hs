module IntCode where 
    import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)
    import Prelude

    type Memory = [Int]
    type Input = [Int]
    type Output = [Int]
    type InstPoint = Int
    data MachineState = MachineState {pointer :: Int, mem :: Memory, input :: [Int], output::[Int], isYield :: Bool} deriving (Show, Eq)
    type OpCode = (Int, [Mode])
    data Mode = Pos | Imm

    getArgs :: Int -> Int-> Memory -> Memory
    getArgs idx count memory = take count (drop idx memory)

    add :: OpCode -> MachineState ->  MachineState
    add (_, modes) (MachineState ip memory input output _) = MachineState (ip + 4) (putMemory resIndex (a + b) memory) input output False
        where
            (aIndex: bIndex: resIndex: _) = getArgs (ip+1) 3 memory
            a = getMemory (modes !! 0) aIndex memory
            b = getMemory (modes !! 1) bIndex memory

    mult :: OpCode -> MachineState ->  MachineState
    mult (_, modes) (MachineState ip memory input output _) =  MachineState (ip + 4) (putMemory resIndex (a * b) memory) input output False
        where
            (aIndex: bIndex: resIndex: _) = getArgs (ip+1) 3 memory
            a = getMemory (modes !! 0) aIndex memory
            b = getMemory (modes !! 1) bIndex memory

    stor :: OpCode -> MachineState -> MachineState
    stor (_, modes) (MachineState ip memory [] output _) = MachineState ip memory [] output True
        where (resIndex : _ ) = getArgs (ip + 1) 2 memory    
    stor (_, modes) (MachineState ip memory (input:restInput) output _) = MachineState (ip + 2) (putMemory resIndex input memory) restInput output False
        where (resIndex : _ ) = getArgs (ip + 1) 2 memory    

    out :: OpCode -> MachineState -> MachineState
    out (_, modes) (MachineState ip memory input output _) = MachineState (ip + 2) memory input (output ++ [getMemory (modes!!0) lookupIdx memory]) False
        where (lookupIdx : _) = getArgs (ip + 1) 1 memory


    jmpt :: OpCode -> MachineState -> MachineState
    jmpt (_, modes) (MachineState ip memory input output _) = MachineState (if test /= 0 then res else ip + 3) memory input output False
        where 
            (a : b : _ ) = getArgs (ip + 1) 3 memory    
            test = getMemory (modes !! 0) a memory
            res = getMemory (modes !! 1) b memory   

    jmpf :: OpCode -> MachineState -> MachineState
    jmpf (_, modes) (MachineState ip memory input output _) = MachineState (if test == 0 then res else ip + 3) memory input output False
        where 
            (a : b : _ ) = getArgs (ip + 1) 3 memory    
            test = getMemory (modes !! 0) a memory
            res = getMemory (modes !! 1) b memory     

    lt :: OpCode -> MachineState -> MachineState
    lt (_, modes) (MachineState ip memory input output _) = MachineState (ip + 4) newMemory input output False
        where 
            (a : b : result : _ ) = getArgs (ip + 1) 4 memory    
            testA = getMemory (modes !! 0) a memory
            testB = getMemory (modes !! 1) b memory    
            newMemory =  putMemory result (if testA < testB then 1 else 0) memory

    equ :: OpCode -> MachineState -> MachineState
    equ (_, modes) (MachineState ip memory input output _) = MachineState (ip + 4) newMemory input output False
        where 
            (a : b : result : _ ) = getArgs (ip + 1) 4 memory    
            testA = getMemory (modes !! 0) a memory
            testB = getMemory (modes !! 1) b memory    
            newMemory =  putMemory result (if testA == testB then 1 else 0) memory            

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
            MachineState pos memory input output _ = state
            op = parseOp (getMemory Pos pos memory)
            (opCode, _) = op

    isComplete :: MachineState -> Bool
    isComplete (MachineState pos memory _ _ yield) = opCode == 99 || yield
        where opCode = getMemory Pos pos memory

    finished :: MachineState -> Bool
    finished (MachineState pos memory _ _ yield) = opCode == 99
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

    getOutput :: MachineState -> [Int]
    getOutput state = output state

    

    intCodeTests = TestList[
        TestCase (assertEqual "getArgs" [2,3,4] (getArgs 1 3 [1,2,3,4,5,6])),
        TestCase (assertEqual "Simple get" 2 (getMemory Pos 1 [1,2,3,4,5,6])),
        TestCase (assertEqual "Simple add " (MachineState 4 [2,0,0,0,99] [0] [] False) (add (1, [Pos, Pos, Pos]) (MachineState 0 [1,0,0,0,99] [0] [] False))),
        TestCase (assertEqual "Simple processStep " (MachineState 4 [2,0,0,0,99] [0] [] False) (processStep (MachineState 0 [1,0,0,0,99] [0] [] False))),
        TestCase (assertEqual "Simple process " (MachineState 4 [2,0,0,0,99] [0] [] False) (process (MachineState 0 [1,0,0,0,99] [0] [] False))),
        TestCase (assertEqual "process " (MachineState 8 [30,1,1,4,2,5,6,0,99] [0] [] False) (process (MachineState 0 [1,1,1,4,99,5,6,0,99] [0] [] False))),
        TestCase (assertEqual "simple mult" (MachineState 4 [2,4,4,5,99,9801] [0] [] False) (process (MachineState 0 [2,4,4,5,99,0] [0] [] False))),
        TestCase (assertEqual "store" (MachineState 2 [3,2,5] [] [] False) (stor (3, [Pos, Pos, Pos]) (MachineState 0 [3,2,0] [5] [] False))),
        TestCase (assertEqual "store empty" (MachineState 0 [3,2,0] [] [] True) (stor (3, [Pos, Pos, Pos]) (MachineState 0 [3,2,0] [] [] False))),
        TestCase (assertEqual "isComplete 99" True (isComplete (MachineState 0 [99] [] [] False))),
        TestCase (assertEqual "finished" True (finished (MachineState 0 [99] [] [] False))),
        TestCase (assertEqual "not finished" False (finished (MachineState 0 [3] [] [] True))),
        TestCase (assertEqual "isComplete yield" True (isComplete (MachineState 0 [3] [] [] True))),
        TestCase (assertEqual "is not Complete" False (isComplete (MachineState 0 [3] [] [] False))),
        TestCase (assertEqual "output" (MachineState 2 [4,2,6] [5] [6] False) (out (4, [Pos, Pos, Pos]) (MachineState 0 [4,2,6] [5] [] False))),
        TestCase (assertEqual "in to out" (MachineState 4 [1,0,4,0,99] [] [1] False) (process (MachineState 0 [3,0,4,0,99] [1] [] False))),
        TestCase (assertEqual "simple full" (MachineState 4 [1101,100,-1,4,99] [1] [] False) (process (MachineState 0 [1101,100,-1,4,0] [1] [] False))),
        TestCase (assertEqual "equal to 8 false" [0] (getOutput (process (MachineState 0 [3,9,8,9,10,9,4,9,99,-1,8] [7] [] False)))),
        TestCase (assertEqual "equal to 8 true" [1] (output (process (MachineState 0 [3,9,8,9,10,9,4,9,99,-1,8] [8] [] False)))),
        TestCase (assertEqual "less than 8 false" [0] (output (process (MachineState 0 [3,9,7,9,10,9,4,9,99,-1,8] [8] [] False)))),
        TestCase (assertEqual "less than 8 true" [1] (output (process (MachineState 0 [3,9,7,9,10,9,4,9,99,-1,8] [7] [] False)))),
        TestCase (assertEqual "imm eq 8 true" [1] (output (process (MachineState 0 [3,3,1108,-1,8,3,4,3,99] [8] [] False)))),
        TestCase (assertEqual "imm eq 8 false" [0] (output (process (MachineState 0 [3,3,1108,-1,8,3,4,3,99] [9] [] False)))),
        TestCase (assertEqual "imm lt 8 true" [1] (output (process (MachineState 0 [3,3,1107,-1,8,3,4,3,99] [7] [] False)))),
        TestCase (assertEqual "imm lt 8 false" [0] (output (process (MachineState 0 [3,3,1107,-1,8,3,4,3,99] [9] [] False)))),
        TestCase (assertEqual "pos jump 1" [1] (output (process (MachineState 0 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [2] [] False)))),
        TestCase (assertEqual "pos jump 0" [0] (output (process (MachineState 0 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0] [] False)))),
        TestCase (assertEqual "im jump 1" [1] (output (process (MachineState 0 [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [9] [] False)))),
        TestCase (assertEqual "im jump 0" [0] (output (process (MachineState 0 [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0] [] False)))),
        TestCase (assertEqual "larger lt 8" [999] (output (process (MachineState 0 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [6] [] False)))),
        TestCase (assertEqual "larger eq 8" [1000] (output (process (MachineState 0 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [8] [] False)))),
        TestCase (assertEqual "larger gt 8" [1001] (output (process (MachineState 0 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [11] [] False))))
        
        ]
