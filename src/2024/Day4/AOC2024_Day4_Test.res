open Jest
open Expect

//open Belt
let data = AOC2024_Day4_Data.data
let sampleData = AOC2024_Day4_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2024_Day4)

describe("2024 Day4", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 18

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 2549

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 9

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 2003

    expect(result)->toEqual(expected)
  })
})
