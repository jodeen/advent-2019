import DayTwoData(part1Data)
import Test.HUnit


type Memory = [Int]
type MachineState = (Int, Memory)

getArgs :: Int -> Int-> Memory -> Memory
getArgs idx count memory = take count (drop idx memory)

add :: Int ->Memory ->  Memory
add idx memory = putMemory resIndex (a + b) memory where
    (aIndex: bIndex: resIndex: _) = getArgs (idx+1) 3 memory
    a = getMemory aIndex memory
    b = getMemory bIndex memory

mult :: Int ->Memory ->  Memory
mult idx memory = putMemory resIndex (a * b) memory where
    (aIndex: bIndex: resIndex: _) = getArgs (idx+1) 3 memory
    a = getMemory aIndex memory
    b = getMemory bIndex memory

getMemory :: Int -> Memory -> Int
getMemory idx memory = memory !! idx

putMemory :: Int -> Int -> Memory -> Memory
putMemory idx value memory = h ++ [value] ++ (tail t) where
    (h, t) = splitAt idx memory

processStep :: MachineState -> MachineState
processStep (pos, memory)
    | opCode == 1 = (pos + 4, add pos memory)
    | opCode == 2 = (pos + 4, mult pos memory)
    | otherwise = error ("Unexpected opcode " ++ (show opCode) ++ " at position " ++ (show pos))
    where opCode = getMemory pos memory

isComplete :: MachineState -> Bool
isComplete (pos, memory)= opCode == 99
    where opCode = getMemory pos memory

process :: MachineState -> MachineState
process state = head (filter isComplete (iterate processStep state))

test1_1 = TestCase (assertEqual "getArgs" [2,3,4] (getArgs 1 3 [1,2,3,4,5,6]))
test1_2 = TestCase (assertEqual "Simple get" 2 (getMemory 1 [1,2,3,4,5,6]))
test1_3 = TestCase (assertEqual "Simple add " [2,0,0,0,99] (add 0 [1,0,0,0,99]))
test1_4 = TestCase (assertEqual "Simple processStep " (4, [2,0,0,0,99]) (processStep (0, [1,0,0,0,99])))
test1_5 = TestCase (assertEqual "Simple process " (4, [2,0,0,0,99]) (process (0, [1,0,0,0,99])))
test1_6 = TestCase (assertEqual "process " (8, [30,1,1,4,2,5,6,0,99]) (process (0, [1,1,1,4,99,5,6,0,99])))
test1_7 = TestCase (assertEqual "simple mult" (4, [2,4,4,5,99,9801]) (process (0, [2,4,4,5,99,0])))

part1Tests = TestList[test1_1, test1_2, test1_3, test1_4, test1_5, test1_6, test1_7]
brokenMem = putMemory 1 12 (putMemory 2 2 part1Data)
part1 = process (0, brokenMem)

processWithModify :: (Int, Int) -> Memory -> (Int, Int, Int) 
processWithModify (noun, verb) memory = (noun, verb, head processedMem)
    where 
        modifiedMem = putMemory 1 noun (putMemory 2 verb memory)
        (_, processedMem) = process (0, modifiedMem)

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

findMatch :: Int -> Memory -> (Int, Int, Int)
findMatch goal memory = head (filter (\(noun, verb, output) -> output == goal ) allResults)
        where
            allPossible = cartProd [0..99] [0..99]
            allResults = map (\(noun, verb) -> processWithModify (noun, verb) memory) allPossible

test2_1 = TestCase (assertEqual "part 1 result" (12, 2, 3760627) (processWithModify (12, 2) part1Data))    
test2_2 = TestCase (assertEqual "part 1 result loop" (12, 2, 3760627) (findMatch 3760627 part1Data))        
part2Tests = TestList[test2_1, test2_2]

part2 = findMatch 19690720 part1Data