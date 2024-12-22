open Jest
open Expect

//open Belt
let data = AOC2024_Day9_Data.data
let sampleData = AOC2024_Day9_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2024_Day9)

describe("2024 Day9", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 1928n

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 6200294120911n

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
