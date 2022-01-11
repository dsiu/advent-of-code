let log = Js.Console.log
let data = AOC2020_Day15_Data.data
let sampleData = AOC2020_Day15_Data_Sample.data

module Log = unpack(JS_Debug.make(__MODULE__))

//let part1 = AOC2020_Day15.solvePart1(sampleData)
let part1 = AOC2020_Day15.solvePart1(data)
Log.debug("Part 1 Result")
part1->log
Log.debug("Part 1 Done")

//let part2 = AOC2020_Day15.solvePart2(sampleData)
let part2 = AOC2020_Day15.solvePart2(data)
Log.debug("Part 2 Result")
part2->log
Log.debug("Part 2 Done")
