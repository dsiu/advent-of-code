@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day4_Data.data
let sampleData = AOC2023_Day4_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day4)

describe("2023 Day4", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 13

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 18619

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 2

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 2

    expect(result)->toEqual(expected)
  })
})
