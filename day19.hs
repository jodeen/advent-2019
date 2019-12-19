import IntCode
import DayData

-- allPoints = map (\y -> [[x,y] | x <- [0..49]]) [0..49]
allPoints = [[x,y] | y <- [0..49], x<-[0..49]]

initialState = stateWithMem day19Data

part1 = sum allStates
    where
        allStates = map (\point -> head (output (process (initialState {input=point})))) allPoints :: [Integer]