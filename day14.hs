import qualified Data.Map.Strict as Map
import Data.Ratio

import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)

type Item = (Int, String)
type ProdMap = Map.Map String (Item, [Item])
type CurrMap = Map.Map String Int

sample1Data = Map.fromList [("A", ((10, "A" ), [(10, "ORE")])),
    ("B", ((1, "B"), [(1, "ORE")])),
    ("C", ((1, "C"), [(7,"A"), (1, "B")])),
    ("D", ((1, "D"), [(7,"A"), (1, "C")])),
    ("E", ((1, "E"), [(7,"A"), (1, "D")])),
    ("FUEL", ((1, "FUEL"), [(7,"A"), (1, "E")]))
    ]

calcCycles :: Int -> Int -> Int
calcCycles needed produced = ceiling ((fromIntegral needed)/ (fromIntegral produced))

calcRequirements :: Item -> ProdMap -> [Item]
calcRequirements (amount, item) prods = map (\(a,i) -> (cycles * a, i)) reqs
    where
        ((prodAmount, _), reqs) = Map.findWithDefault ((amount, item), [(amount, item)]) item prods
        cycles = calcCycles amount prodAmount
        
 

combine :: [Item] -> [Item]
combine items = Map.elems combined
    where combined = foldl (\m (amount, name) -> Map.alter (combineItems (amount, name)) name m) Map.empty items


combineItems :: Item -> Maybe Item -> Maybe Item
combineItems (a, name) (Just (b, _)) = Just (a+b, name)
combineItems (a, name) Nothing = Just (a, name)

findNeeds :: [Item] -> CurrMap -> [Item]
findNeeds requirements current = filter (\(a,_) -> a /= 0) needs
    where
        needs = map (\(a,i) -> (max (a - (Map.findWithDefault 0 i current)) 0, i)) requirements

expand :: [Item] -> ProdMap -> [Item]
expand (item:items) prods = (calcRequirements item prods) ++ (expand items prods)
expand [] prods = []

doStep :: ProdMap -> [Item] -> [Item]
doStep prods item = combined
    where 
        expanded = expand item prods
        combined = combine expanded

part1Tests = TestList[
    TestCase (assertEqual "simple calc" [(7, "A"), (1,"B")] (calcRequirements (1,"C") sample1Data)),
    TestCase (assertEqual "simple calc mult" [(14, "A"), (2,"B")] (calcRequirements (2,"C") sample1Data)),
    TestCase (assertEqual "simple calc ore " [(10, "ORE")] (calcRequirements (10,"A") sample1Data)),
    TestCase (assertEqual "simple calc ore " [(10, "ORE")] (calcRequirements (1,"A") sample1Data)),
    TestCase (assertEqual "simple calc ore " [(4, "ORE")] (calcRequirements (4,"A") (Map.fromList  [("A", ((3, "A" ), [(2, "ORE")]))] ))),
    TestCase (assertEqual "simple combine " [(10, "ORE")] (combine [(10, "ORE")])),
    TestCase (assertEqual "simple combine " [(10, "ORE")] (combine [(5, "ORE"), (5, "ORE")])),
    TestCase (assertEqual "simple combine " [ (7, "A"), (10, "ORE")] (combine [(5, "ORE"), (7, "A"), (5, "ORE")])),
    TestCase (assertEqual "simple combine " [(7,"A"),(1,"B"),(30,"ORE")] (combine [(1,"B"),(7,"A"),(10,"ORE"),(20,"ORE")])),

    -- TestCase (assertEqual "find needs " [(10, "ORE"), (7, "A")] (findNeeds [(5, "ORE"), (7, "A"), (5, "ORE")])),
    TestCase (assertEqual "dummy" True (True))
    ]



