import IntCode
import DayNineData
import qualified Data.Map.Strict as Map


part1 = process (emptyState {mem=(part1Data, Map.empty), input=[1]})

part2 = process (emptyState {mem=(part1Data, Map.empty), input=[2]})