@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day10_Data.data
let sampleData = AOC2023_Day10_Data_Sample.data
let sampleData1 = AOC2023_Day10_Data_Sample.data1
let sampleData2 = AOC2023_Day10_Data_Sample.data2
let sampleData3 = AOC2023_Day10_Data_Sample.data3
let sampleData4 = AOC2023_Day10_Data_Sample.data4
let sampleData5 = AOC2023_Day10_Data_Sample.data5

let {solvePart1, solvePart2, parse, followPath} = module(AOC2023_Day10)
module Map = AOC2023_Day10.Map

describe("2023 Day10", () => {
  let map = sampleData->parse->Map.make
  let map1 = sampleData1->parse->Map.make
  let map2 = sampleData2->parse->Map.make
  let map3 = sampleData3->parse->Map.make

  describe("findPath", () => {
    test(
      "data and data1 should have same path",
      () => {
        let pathA = map->followPath(map.start)
        let pathB = map1->followPath(map1.start)

        expect(pathA)->toEqual(pathB)
      },
    )

    test(
      "data2 and data3 should have same path",
      () => {
        let pathA = map2->followPath(map2.start)
        let pathB = map3->followPath(map3.start)

        expect(pathA)->toEqual(pathB)
      },
    )
  })
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 4

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Sample Data 1", () => {
    let result = solvePart1(sampleData1)
    let expected = 4

    expect(result)->toEqual(expected)
  })
  test("Part 1 - Sample Data 2", () => {
    let result = solvePart1(sampleData1)
    let expected = 4

    expect(result)->toEqual(expected)
  })
  test("Part 1 - Sample Data 3", () => {
    let result = solvePart1(sampleData1)
    let expected = 4

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 7005

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 1

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data 4", () => {
    let result = solvePart2(sampleData4)
    let expected = 4

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data 5", () => {
    let result = solvePart2(sampleData5)
    let expected = 10

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 417

    expect(result)->toEqual(expected)
  })
})
