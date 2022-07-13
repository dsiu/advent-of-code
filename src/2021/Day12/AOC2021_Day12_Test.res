open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day12_Data.data
let sampleData = AOC2021_Day12_Data_Sample.data
let sampleData0 = AOC2021_Day12_Data_Sample.data0
let sampleData1 = AOC2021_Day12_Data_Sample.data1

describe("2021 Day12", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day12.solvePart1(sampleData)
    let expected = 226

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Sample Data 0", () => {
    let result = AOC2021_Day12.solvePart1(sampleData0)
    let expected = 10

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Sample Data 1", () => {
    let result = AOC2021_Day12.solvePart1(sampleData1)
    let expected = 19

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day12.solvePart1(data)
    let expected = 4338

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day12.solvePart2(sampleData)
    let expected = 3509

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data 0", () => {
    let result = AOC2021_Day12.solvePart2(sampleData0)
    let expected = 36

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data 1", () => {
    let result = AOC2021_Day12.solvePart2(sampleData1)
    let expected = 103

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day12.solvePart2(data)
    let expected = 114189

    expect(result)->toEqual(expected)
  })
})
