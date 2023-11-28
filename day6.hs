import Data.Tree
import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)
import DaySixData


-- data Tree = Tree (Node, [Tree]) | Leaf Node
--     deriving (Show)
-- type Node = (String, Int)
type Orbit = (String, String)

-- create :: Node -> Tree
-- create node = Leaf node

-- findByLabel :: String -> Tree -> Tree
-- findByLabel label Leaf (label, _)= Leaf 1


        
findChildren :: [Orbit] -> String -> (String, [String])
findChildren orbits parent = (parent, children) 
    where
        children = map snd (filter (\(a,b) -> a == parent) orbits)
   
createTree :: [Orbit] -> Tree String
createTree orbits = unfoldTree (findChildren orbits) "COM"

sampleOrbits = map parseInput [
    "COM)B",
    "B)C",
    "C)D",
    "D)E",
    "E)F",
    "B)G",
    "G)H",
    "D)I",
    "E)J",
    "J)K",
    "K)YOU",
    "I)SAN",
    "K)L"]

sampleTree = createTree sampleOrbits

calcOrbits :: [Orbit] -> Int
calcOrbits orbits = sum  (map (\(a,b) -> a*b) levelCount)
    where 
        tree = createTree orbits
        levelCount = zip ((map length) (levels tree)) [0..]


part1Tests = TestList[
    TestCase (assertEqual "parse" ("ABC","DEF") (parseInput "ABC)DEF"))
    ]

part1 = calcOrbits part1Data

contains :: String -> Tree String -> Bool
contains item tree = (item == rootLabel tree) || any (contains item) (subForest tree)

parent :: String -> [Orbit] -> String
parent child orbits = foundParent
    where (foundParent, _) = head (filter (\(_,b) -> b == child) orbits)

smallestSubtree :: String -> String -> Tree String -> Tree String
smallestSubtree a b tree = if (childContains == []) then tree else smallestSubtree a b (head childContains)
        where childContains = filter (\t -> (contains a t) && (contains b t)) (subForest tree)

pathTo :: String -> Tree String -> [String]
pathTo label tree = if (rootLabel tree == label) 
    then [rootLabel tree] 
    else [rootLabel tree] ++ (pathTo label (head ((filter (contains label) (subForest tree)))))

minDist :: String -> String -> [Orbit] -> Int
minDist a b orbits = (length aPath - 1) + (length bPath -1 )
    where 
        aParent = parent a orbits
        bParent = parent b orbits
        tree = createTree orbits
        subTree = smallestSubtree aParent bParent tree
        aPath = pathTo aParent subTree
        bPath = pathTo bParent subTree

part2 = minDist "YOU" "SAN" part1Data