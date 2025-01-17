open Jest
open Expect

//open Belt
let data = AOC2023_Day15_Data.data
let sampleData = AOC2023_Day15_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day15)

describe("2023 Day15", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 1320

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 515495

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 145

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 229349

    expect(result)->toEqual(expected)
  })
})
