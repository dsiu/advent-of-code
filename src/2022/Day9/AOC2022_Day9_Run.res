let log = Js.Console.log
let data = AOC2022_Day9_Data.data
let sampleData = AOC2022_Day9_Data_Sample.data
let sampleData1 = AOC2022_Day9_Data_Sample.data1
let {solvePart1, solvePart2} = module(AOC2022_Day9)

let t1 = Js.Date.now()

//let part1 = solvePart1(sampleData)
let part1 = solvePart1(data)
let t1_done = Js.Date.now()

"Part 1 Result"->log
part1->log
`time : ${MS.fromMS(t1_done -. t1)}`->log

"----------"->log

let t2 = Js.Date.now()

let part2 = solvePart2(sampleData1)
//let part2 = solvePart2(data)
let t2_done = Js.Date.now()

"Part 2 Result"->log
part2->log
`time : ${MS.fromMS(t1_done -. t1)}`->log
