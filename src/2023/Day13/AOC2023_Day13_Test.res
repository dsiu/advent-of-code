@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day13_Data.data
let sampleData = AOC2023_Day13_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day13)

describe("2023 Day13", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 405

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 43614

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 400

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 36771

    expect(result)->toEqual(expected)
  })
})
