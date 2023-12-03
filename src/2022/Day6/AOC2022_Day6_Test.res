open Jest
open Expect
open Jest2

//open Belt
let data = AOC2022_Day6_Data.data
let sampleData = AOC2022_Day6_Data_Sample.data
let sampleData1 = AOC2022_Day6_Data_Sample.data1
let sampleData2 = AOC2022_Day6_Data_Sample.data2
let sampleData3 = AOC2022_Day6_Data_Sample.data3
let sampleData4 = AOC2022_Day6_Data_Sample.data4

let {solvePart1, solvePart2} = module(AOC2022_Day6)

describe("2022 Day6", () => {
  let part1SampleData = [
    (sampleData->solvePart1, 7),
    (sampleData1->solvePart1, 5),
    (sampleData2->solvePart1, 6),
    (sampleData3->solvePart1, 10),
    (sampleData4->solvePart1, 11),
  ]

  testEach2("Part 1 - Sample Data", part1SampleData, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 1140

    expect(result)->toEqual(expected)
  })

  let part2SampleData = [
    (sampleData->solvePart2, 19),
    (sampleData1->solvePart2, 23),
    (sampleData2->solvePart2, 23),
    (sampleData3->solvePart2, 29),
    (sampleData4->solvePart2, 26),
  ]

  testEach2("Part 2 - Sample Data", part2SampleData, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 3495

    expect(result)->toEqual(expected)
  })
})
