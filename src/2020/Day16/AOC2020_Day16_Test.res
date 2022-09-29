open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day16_Data.data
let sampleData1 = AOC2020_Day16_Data_Sample.data1
let sampleData2 = AOC2020_Day16_Data_Sample.data2

describe("2020 Day16", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day16.solvePart1(sampleData1)
    let expected = 71

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day16.solvePart1(data)
    let expected = 28882

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2020_Day16.solvePart2(sampleData2)
    let expected = 2

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day16.solvePart2(data)
    let expected = 2

    expect(result)->toEqual(expected)
  })
})
