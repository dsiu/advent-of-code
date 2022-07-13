open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day8_Data.data
let sampleData = AOC2020_Day8_Data_Sample.data

describe("2020 Day8", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day8.solvePart1(sampleData)
    let expected = 5

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day8.solvePart1(data)
    let expected = 1394

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day8.solvePart2(data)
    let expected = 1626

    expect(result)->toEqual(expected)
  })
})
