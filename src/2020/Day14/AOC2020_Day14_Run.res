let log = Js.Console.log
let data = AOC2020_Day14_Data.data
let sampleData = AOC2020_Day14_Data_Sample.data
let sampleData2 = AOC2020_Day14_Data_Sample.data2

let part1 = AOC2020_Day14.solvePart1(sampleData)
//let part1 = AOC2020_Day14.solvePart1(data)
"Part 1 Result"->log
part1->log

let part2 = AOC2020_Day14.solvePart2(sampleData2)
"Part 2 Result"->log
part2->log

"============"->log
open AOC2020_Day14

let mask = Program.Mask.make("000000000000000000000000000000X1001X")
mask->Program.Mask.dump
let address = Int64.of_string("0u42")
let value = Int64.of_string("0u100")

Program.decode_memory(mask, address, value)->log
