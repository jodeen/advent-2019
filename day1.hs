import DayOneData(part1Data)

calcFuel :: Integer -> Integer
calcFuel mass = mass `div` 3 - 2

calcModules :: [Integer] -> Integer
calcModules modules = sum (map calcFuel modules)

part1 = calcModules part1Data

calcFullFuel:: Integer -> Integer
calcFullFuel mass
    | newFuel <= 0  = 0
    | otherwise = newFuel + (calcFullFuel newFuel)
    where newFuel = calcFuel mass

calcFullModules :: [Integer] -> Integer
calcFullModules modules = sum (map calcFullFuel modules)

part2 = calcFullModules part1Data

