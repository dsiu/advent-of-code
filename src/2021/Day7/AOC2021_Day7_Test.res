open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day7_Data.data
let sampleData = AOC2021_Day7_Data_Sample.data

describe("2021 Day7", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day7.solvePart1(sampleData)
    let expected = 37

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day7.solvePart1(data)
    let expected = 331_067

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day7.solvePart2(sampleData)
    let expected = 168

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day7.solvePart2(data)
    let expected = 92_881_128

    expect(result)->toEqual(expected)
  })
})
