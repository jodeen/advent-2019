import Test.HUnit (Test(TestCase), Test(TestList), assertEqual, runTestTT)


type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
type Moon = (Position, Velocity)


gravity :: Moon -> Moon -> Moon
gravity ((aX, aY, aZ), (vX, vY, vZ)) ((bX, bY, bZ), bV) = ((aX, aY, aZ), 
    (vX + computePosChange aX bX, 
    vY + computePosChange aY bY, 
    vZ + computePosChange aZ bZ))

computePosChange :: Int -> Int -> Int
computePosChange a b 
    | a < b = 1 
    | a == b = 0
    | otherwise = -1

applyVelocity:: Moon -> Moon
applyVelocity ((x, y, z), (vX, vY, vZ)) = ((x+vX, y+vY, z+vZ), (vX, vY, vZ))

doStepSingle :: Moon -> [Moon] -> Moon
doStepSingle moon moons = applyVelocity (foldl gravity moon moons)

sample1 = [((-1,0,2), (0,0,0)),((2,-10,-7), (0,0,0)),((4,-8,8), (0,0,0)),((3,5,-1), (0,0,0))] ::[Moon]

doStep :: [Moon] -> [Moon]
doStep moons = map (\m -> doStepSingle m moons) moons

singleEnergy :: Moon -> Int
singleEnergy ((x,y,z), (vX, vY, vZ)) = pot * kin
    where 
        pot = (abs x) + (abs y) + (abs z)
        kin = (abs vX) + (abs vY) + (abs vZ)

energy :: [Moon] -> Int
energy moons = sum (map singleEnergy moons)

part1Tests = TestList[
    TestCase (assertEqual "computePosChange" 1 (computePosChange 3 5)),
    TestCase (assertEqual "computePosChange" 0 (computePosChange 5 5)),
    TestCase (assertEqual "computePosChange" (-1) (computePosChange 5 3)),
    TestCase (assertEqual "gravity" ((0,0,0), (0,0,0)) (gravity ((0,0,0), (0,0,0)) ((0,0,0), (0,0,0)) )),
    TestCase (assertEqual "gravity" ((0,0,0), (3,2,1)) (gravity ((0,0,0), (3,2,1)) ((0,0,0), (3,2,1)) )),
    TestCase (assertEqual "gravity" ((5,0,0), (-1,0,0)) (gravity ((5,0,0), (0,0,0)) ((0,0,0), (0,0,0)) )),
    TestCase (assertEqual "gravity complex" ((5,2,6), (1,3,2)) (gravity ((5,2,6), (2,2,2)) ((1,4,6), (0,0,0)) )),

    TestCase (assertEqual "dummy" (True) (True))
    ]

part1Data = [((3, 15, 8), (0,0,0)), ((5,-1,-2), (0,0,0)), ((-10, 8, 2), (0,0,0)), ((8,4,-5), (0,0,0))]

part1 = energy (last (take 1001 (iterate doStep part1Data)))