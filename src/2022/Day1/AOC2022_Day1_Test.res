open Jest
open Expect

//open Belt
let data = AOC2022_Day1_Data.data
let sampleData = AOC2022_Day1_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2022_Day1)

describe("2022 Day1", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 24000

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 64929

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 45000

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 193697

    expect(result)->toEqual(expected)
  })
})
