open Jest
open Expect

//open Belt
let data = AOC2023_Day16_Data.data
let sampleData = AOC2023_Day16_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day16)

describe("2023 Day16", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 46

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 6883

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 51

    expect(result)->toEqual(expected)
  })

  Skip.test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 7228

    expect(result)->toEqual(expected)
  })
})
