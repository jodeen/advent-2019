module IntCode where 
    import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)
    import Prelude
    import qualified Data.Map.Strict as Map

    type Memory = ([Integer], Map.Map Integer Integer)
    type Input = [Integer]
    type Output = [Integer]
    type InstPoint = Integer
    data MachineState = MachineState {
        pointer :: Integer, 
        mem :: Memory, 
        input :: [Integer], 
        output::[Integer], 
        isYield :: Bool,
        relativeBase :: Integer} deriving (Show, Eq)
    type OpCode = (Integer, [Mode])
    data Mode = Pos | Imm | Rel deriving (Eq, Show)

    getArgs :: Integer -> Integer-> Memory -> [Integer]
    getArgs idx count memory = map (\i -> getMemory i memory) [idx .. idx+count-1]

    binaryOp :: (Integer -> Integer -> Integer) -> OpCode -> MachineState ->  MachineState
    binaryOp binaryFunction (_, modes) state = state {pointer = ip + 4, mem = newMemory, isYield = False}
        where
            MachineState {pointer = ip, mem =memory} = state
            (aIndex: bIndex: resIndex: _) = getArgs (ip+1) 3 memory
            a = getArgValue (modes !! 0) aIndex state
            b = getArgValue (modes !! 1) bIndex state
            newMemory = putMemory (modes !! 2) resIndex (binaryFunction a b) state

    add :: OpCode -> MachineState ->  MachineState
    add = binaryOp (+)

    mult :: OpCode -> MachineState ->  MachineState
    mult = binaryOp (*)

    stor :: OpCode -> MachineState -> MachineState
    stor (_, modes) state | (input state) == []  = state {isYield = True}
    stor (_, modes) state = state {pointer = ip + 2, mem=newMemory, input = restInput, isYield = False}
        where 
            MachineState {pointer = ip, input = (input: restInput), mem=memory} = state
            (resIndex : _ ) = (getArgs (ip + 1) 2 memory ) 
            newMemory = putMemory (modes !! 0) resIndex input state
            

    out :: OpCode -> MachineState -> MachineState
    out (_, modes) state = state {pointer = ip + 2, output = output ++ [getArgValue (modes!!0) lookupIdx state], isYield = False}
        where 
            MachineState {pointer = ip, mem=memory, output=output} = state
            (lookupIdx : _) = getArgs (ip + 1) 1 memory


    jmpt :: OpCode -> MachineState -> MachineState
    jmpt (_, modes) state = state {pointer = if test /= 0 then res else ip + 3, isYield = False}
        where 
            MachineState {pointer=ip, mem=memory} = state
            (a : b : _ ) = getArgs (ip + 1) 3 memory    
            test = getArgValue (modes !! 0) a state
            res = getArgValue (modes !! 1) b state   

    jmpf :: OpCode -> MachineState -> MachineState
    jmpf (_, modes) state = state {pointer = if test == 0 then res else ip + 3, isYield = False}
        where 
            MachineState {pointer=ip, mem=memory} = state
            (a : b : _ ) = getArgs (ip + 1) 3 memory    
            test = getArgValue (modes !! 0) a state
            res = getArgValue (modes !! 1) b state     

    lt :: OpCode -> MachineState -> MachineState
    lt (_, modes) state = state {pointer = ip + 4, mem=newMemory, isYield = False}
        where 
            MachineState {pointer=ip, mem=memory} = state
            (a : b : result : _ ) = getArgs (ip + 1) 4 memory    
            testA = getArgValue (modes !! 0) a state
            testB = getArgValue (modes !! 1) b state    
            newMemory =  putMemory (modes !!2) result (if testA < testB then 1 else 0) state

    equ :: OpCode -> MachineState -> MachineState
    equ (_, modes) state = state {pointer = ip + 4, mem=newMemory, isYield = False}
        where 
            MachineState {pointer=ip, mem=memory} = state
            (a : b : result : _ ) = getArgs (ip + 1) 4 memory    
            testA = getArgValue (modes !! 0) a state
            testB = getArgValue (modes !! 1) b state    
            newMemory =  putMemory (modes !!2) result (if testA == testB then 1 else 0) state            

    arel :: OpCode -> MachineState -> MachineState
    arel (_, modes) state = state {pointer = ip + 2, isYield = False, relativeBase = (relativeBase state) + value}
        where 
            MachineState {pointer = ip, mem=memory, output=output} = state
            (lookupIdx : _) = getArgs (ip + 1) 1 memory     
            value =      getArgValue (modes!!0) lookupIdx state   


    getArgValue :: Mode -> Integer ->  MachineState -> Integer
    getArgValue Pos idx MachineState {mem=memory} = getMemory idx memory
    getArgValue Imm value _ = value
    getArgValue Rel value MachineState {mem=memory, relativeBase = base} = getMemory (base + value) memory

    getMemory :: Integer -> Memory -> Integer
    getMemory idx (memory, ext) 
        |(fromIntegral idx) < length memory = memory !! (fromIntegral idx)
        | otherwise = case (Map.lookup idx ext) of 
            Just(value) -> value
            Nothing -> 0
        where 
            (h, t) = splitAt (fromIntegral idx) memory            
       
    putMemory :: Mode -> Integer -> Integer -> MachineState -> Memory
    putMemory Pos idx value MachineState {mem=(memory, ext)}
         |(fromIntegral idx) < length memory = (h ++ [value] ++ (tail t), ext)
         | otherwise = (memory, Map.insert idx value ext)
         where (h, t) = splitAt (fromIntegral idx) memory
    putMemory Rel idx value MachineState {mem=(memory, ext), relativeBase=relativeBase}
         |(fromIntegral endIndex) < length memory = (h ++ [value] ++ (tail t), ext)
         | otherwise = (memory, Map.insert endIndex value ext)
         where 
            endIndex = idx + relativeBase
            (h, t) = splitAt (fromIntegral endIndex) memory
            
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
        | opCode == 9 = arel op state
        | otherwise = error ("Unexpected opcode " ++ (show opCode) ++ " at position " ++ (show ip))
        where 
            MachineState{pointer = ip, mem=memory} = state
            op = parseOp (getArgValue Pos ip state)
            (opCode, _) = op

    isComplete :: MachineState -> Bool
    isComplete state  = opCode == 99 || isYield state
    
        where opCode = getArgValue Pos (pointer state) state

    finished :: MachineState -> Bool
    finished state= opCode == 99
        where opCode = getArgValue Pos (pointer state) state
    
    lpad :: Integer -> Integer -> String
    lpad num len = reverse (take (fromIntegral len) (reverse ((replicate (fromIntegral len) '0') ++ (show num))))

    numToMode :: Char -> Mode
    numToMode '0' = Pos
    numToMode '1' = Imm
    numToMode '2' = Rel

    parseOp :: Integer -> OpCode
    parseOp op = (read [d,e], [numToMode c,numToMode b, numToMode a])
        where
            (a: b: c: d: e: _) = lpad op 5

    process :: MachineState -> MachineState
    process state = head (filter isComplete (iterate processStep state))

    getOutput :: MachineState -> [Integer]
    getOutput state = output state

    emptyState = (MachineState 0 ([], Map.empty) [] [] False 0)
    
    stateWithMem :: [Integer] -> MachineState
    stateWithMem m = emptyState {mem = (m, Map.empty)}

    intCodeTests = TestList[
        TestCase (assertEqual "parseOp default" (4, [Pos, Pos, Pos]) (parseOp 4)),
        TestCase (assertEqual "parseOp " (4, [Rel, Imm, Pos]) (parseOp 1204)),
        TestCase (assertEqual "parseOp " (4, [Rel, Pos, Imm]) (parseOp 10204)),
        TestCase (assertEqual "getArgs" [2,3,4] (getArgs 1 3 ([1,2,3,4,5,6], Map.empty))),
        TestCase (assertEqual "Simple get" 2 (getArgValue Pos 1 (emptyState {mem=([1,2,3,4,5,6], Map.empty)}))),
        TestCase (assertEqual "Simple add " (MachineState 4 ([2,0,0,0,99], Map.empty) [0] [] False 0) (add (1, [Pos, Pos, Pos]) (MachineState 0 ([1,0,0,0,99], Map.empty) [0] [] False 0))),
        TestCase (assertEqual "Simple processStep " (MachineState 4 ([2,0,0,0,99], Map.empty) [0] [] False 0) (processStep (MachineState 0 ([1,0,0,0,99], Map.empty) [0] [] False 0))),
        TestCase (assertEqual "Simple process " (MachineState 4 ([2,0,0,0,99], Map.empty) [0] [] False 0) (process (MachineState 0 ([1,0,0,0,99], Map.empty) [0] [] False 0))),
        TestCase (assertEqual "process " (MachineState 8 ([30,1,1,4,2,5,6,0,99], Map.empty) [0] [] False 0) (process (MachineState 0 ([1,1,1,4,99,5,6,0,99], Map.empty) [0] [] False 0))),
        TestCase (assertEqual "simple mult" (MachineState 4 ([2,4,4,5,99,9801], Map.empty) [0] [] False 0) (process (MachineState 0 ([2,4,4,5,99,0], Map.empty) [0] [] False 0))),
        TestCase (assertEqual "store" (MachineState 2 ([3,2,5], Map.empty) [] [] False 0) (stor (3, [Pos, Pos, Pos]) (MachineState 0 ([3,2,0], Map.empty) [5] [] False 0))),
        TestCase (assertEqual "store empty" (MachineState 0 ([3,2,0], Map.empty) [] [] True 0) (stor (3, [Pos, Pos, Pos]) (MachineState 0 ([3,2,0], Map.empty) [] [] False 0))),
        TestCase (assertEqual "store rel" (MachineState 2 ([203, 10], Map.fromList [(10, 15)]) [] [] False 0) (processStep (stateWithMem [203, 10]) {input=[15]})),
        TestCase (assertEqual "store rel 2" (MachineState 2 ([203, 10], Map.fromList [(20, 15)]) [] [] False 10) (processStep (stateWithMem [203, 10]) {input=[15], relativeBase=10})),
        TestCase (assertEqual "simple base set" (MachineState 2 ([9,3,0,10], Map.empty) [] [] False 10) (arel (3, [Pos, Pos, Pos]) (MachineState 0 ([9,3,0,10], Map.empty) [] [] False 0))),
        TestCase (assertEqual "simple base set immediate" (MachineState 2 ([109,10], Map.empty) [] [] False 10) (arel (3, [Imm, Pos, Pos]) (MachineState 0 ([109,10], Map.empty) [] [] False 0))),        
        TestCase (assertEqual "isComplete 99" True (isComplete (MachineState 0 ([99], Map.empty) [] [] False 0))),
        TestCase (assertEqual "finished" True (finished (MachineState 0 ([99], Map.empty) [] [] False 0))),
        TestCase (assertEqual "not finished" False (finished (MachineState 0 ([3], Map.empty) [] [] True 0))),
        TestCase (assertEqual "isComplete yield" True (isComplete (MachineState 0 ([3], Map.empty) [] [] True 0))),
        TestCase (assertEqual "is not Complete" False (isComplete (MachineState 0 ([3], Map.empty) [] [] False 0))),
        TestCase (assertEqual "output" (MachineState 2 ([4,2,6], Map.empty) [5] [6] False 0) (out (4, [Pos, Pos, Pos]) (MachineState 0 ([4,2,6], Map.empty) [5] [] False 0))),
        TestCase (assertEqual "in to out" (MachineState 4 ([1,0,4,0,99], Map.empty) [] [1] False 0) (process (MachineState 0 ([3,0,4,0,99], Map.empty) [1] [] False 0))),
        TestCase (assertEqual "simple full" (MachineState 4 ([1101,100,-1,4,99], Map.empty) [1] [] False 0) (process (MachineState 0 ([1101,100,-1,4,0], Map.empty) [1] [] False 0))),
        TestCase (assertEqual "equal to 8 false" [0] (getOutput (process (MachineState 0 ([3,9,8,9,10,9,4,9,99,-1,8], Map.empty) [7] [] False 0)))),
        TestCase (assertEqual "equal to 8 true" [1] (output (process (MachineState 0 ([3,9,8,9,10,9,4,9,99,-1,8], Map.empty) [8] [] False 0)))),
        TestCase (assertEqual "less than 8 false" [0] (output (process (MachineState 0 ([3,9,7,9,10,9,4,9,99,-1,8], Map.empty) [8] [] False 0)))),
        TestCase (assertEqual "less than 8 true" [1] (output (process (MachineState 0 ([3,9,7,9,10,9,4,9,99,-1,8], Map.empty) [7] [] False 0)))),
        TestCase (assertEqual "imm eq 8 true" [1] (output (process (MachineState 0 ([3,3,1108,-1,8,3,4,3,99] , Map.empty)[8] [] False 0)))),
        TestCase (assertEqual "imm eq 8 false" [0] (output (process (MachineState 0 ([3,3,1108,-1,8,3,4,3,99], Map.empty) [9] [] False 0)))),
        TestCase (assertEqual "imm lt 8 true" [1] (output (process (MachineState 0 ([3,3,1107,-1,8,3,4,3,99], Map.empty) [7] [] False 0)))),
        TestCase (assertEqual "imm lt 8 false" [0] (output (process (MachineState 0 ([3,3,1107,-1,8,3,4,3,99], Map.empty) [9] [] False 0)))),
        TestCase (assertEqual "pos jump 1" [1] (output (process (MachineState 0 ([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], Map.empty) [2] [] False 0)))),
        TestCase (assertEqual "pos jump 0" [0] (output (process (MachineState 0 ([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], Map.empty) [0] [] False 0)))),
        TestCase (assertEqual "im jump 1" [1] (output (process (MachineState 0 ([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], Map.empty) [9] [] False 0)))),
        TestCase (assertEqual "im jump 0" [0] (output (process (MachineState 0 ([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], Map.empty) [0] [] False 0)))),
        TestCase (assertEqual "larger lt 8" [999] (output (process (MachineState 0 ([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], Map.empty) [6] [] False 0)))),
        TestCase (assertEqual "larger eq 8" [1000] (output (process (MachineState 0 ([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], Map.empty) [8] [] False 0)))),
        TestCase (assertEqual "larger gt 8" [1001] (output (process (MachineState 0 ([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], Map.empty) [11] [] False 0)))),

        TestCase (assertEqual "simple put mem" ([0,1], Map.empty) (putMemory Pos 1 1 (stateWithMem [0, 0]))),
        TestCase (assertEqual "simple put mem" ([0], Map.fromList [(1,1)]) (putMemory Pos 1 1 (stateWithMem [0]))),
        TestCase (assertEqual "simple get mem" 0 (getMemory 0 ([0], Map.empty))),
        TestCase (assertEqual "simple get mem ext default" 0 (getMemory 10 ([0], Map.empty))),
        TestCase (assertEqual "simple get mem ext" 4 (getMemory 10 ([0], Map.fromList [(10, 4)]))),
        TestCase (assertEqual "sample 1" [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] (output (process (emptyState {mem=([109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99], Map.empty)})))),
        TestCase (assertEqual "sample 2" [1219070632396864] (output (process (emptyState {mem=([1102,34915192,34915192,7,4,7,99,0], Map.empty)})))),
        TestCase (assertEqual "sample 3" [1125899906842624] (output (process (emptyState {mem=([104,1125899906842624,99], Map.empty)}))))        
        -- TestCase (assertEqual "Rel mode " (MachineState 4 [204,-34] [0] [] False 0) (processStep (MachineState 0 [204,-34] [0] [] False 2019))),        
        ]
