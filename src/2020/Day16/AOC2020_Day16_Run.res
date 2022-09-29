let log = Js.Console.log
let data = AOC2020_Day16_Data.data
let sampleData1 = AOC2020_Day16_Data_Sample.data1
let sampleData2 = AOC2020_Day16_Data_Sample.data2
let {solvePart1, solvePart2} = module(AOC2020_Day16)

let t1 = Js.Date.now()

//let part1 = solvePart1(sampleData1)
let part1 = solvePart1(data)
let t1_done = Js.Date.now()

"Part 1 Result"->log
part1->log
`time : ${MS.fromMS(t1_done -. t1)}`->log

"----------"->log

let t2 = Js.Date.now()

let part2 = solvePart2(sampleData2)
//let part2 = solvePart2(data)
let t2_done = Js.Date.now()

"Part 2 Result"->log
part2->log
`time : ${MS.fromMS(t1_done -. t1)}`->log
