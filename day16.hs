import Data.List
import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)


parseInput :: String -> [Int]
parseInput s = map (\c -> read [c]) s

fft :: [Int] -> [Int] -> Int
fft input pattern = onesDigit summed
    where 
        summed = sum (zipWith (*) input pattern)

onesDigit :: Int -> Int
onesDigit num =  mod (abs num) 10

basePattern = [0,1,0,-1]

calcPattern :: [Int] -> Int -> [Int]
calcPattern base elem = tail (cycle (concat (map (\i -> replicate elem i) base)))

calcPart1 :: [Int] -> [Int]
calcPart1 input = pairs
    where
        pairs = map (\n -> fft input (calcPattern basePattern n)) [1..(length input)]

calcPart1Total :: [Int] -> [Int]
calcPart1Total input = head (drop 100 (iterate calcPart1 input))

part1Data = parseInput "59772698208671263608240764571860866740121164692713197043172876418614411671204569068438371694198033241854293277505547521082227127768000396875825588514931816469636073669086528579846568167984238468847424310692809356588283194938312247006770713872391449523616600709476337381408155057994717671310487116607321731472193054148383351831456193884046899113727301389297433553956552888308567897333657138353770191097676986516493304731239036959591922009371079393026332649558536888902303554797360691183681625604439250088062481052510016157472847289467410561025668637527408406615316940050060474260802000437356279910335624476330375485351373298491579364732029523664108987"

part1 = take 8 (calcPart1Total part1Data)
    
part1Tests = TestList [
    TestCase (assertEqual "parseInput" [1,2,3] (parseInput "123")),    
    TestCase (assertEqual "calcPattern" [0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1, 0] (take 12 (calcPattern basePattern 3))),        
    TestCase (assertEqual "calcPattern" [1,0,-1,0,1] (take 5 (calcPattern basePattern 1))),        
    TestCase (assertEqual "fft" 4 (onesDigit (fft [1..8] [1, 0, -1, 0, 1, 0, -1, 0]))),            
    TestCase (assertEqual "fft" 2 (onesDigit (fft [1..8] [0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1, 0]))),            
    TestCase (assertEqual "sample 1" [4,8,2,2,6,1,5,8] (calcPart1 [1..8])),            
    TestCase (assertEqual "sample 1" [3,4,0,4,0,4,3,8] (calcPart1 [4,8,2,2,6,1,5,8])),            
    TestCase (assertEqual "dummy" True (True))
    ]

listToInt :: [Int] -> Int
listToInt ints = read (concat (map show ints))

calcPart2Total :: [Int] -> [Int]
calcPart2Total input = calcLastHalf offset 100 fullInput
    where 
        fullInput = concat (replicate 10000 input)
        offset = listToInt (take 7 input)

calcOffset :: [Int] -> Int
calcOffset input = listToInt (take 7 input)

calcSimple :: [Int] -> [Int]
calcSimple input = map (\l -> onesDigit (sum l)) (init (tails input))

calcLastHalf :: Int -> Int -> [Int] -> [Int]
calcLastHalf offset rounds fullInput = head (drop rounds (iterate calcSimple selection))
    where
        selection = drop offset fullInput