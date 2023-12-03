open Jest
open Expect

//open Belt
let data = AOC2022_Day9_Data.data
let sampleData = AOC2022_Day9_Data_Sample.data
let sampleData1 = AOC2022_Day9_Data_Sample.data1
let {solvePart1, solvePart2} = module(AOC2022_Day9)

describe("2022 Day9", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 13

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 5874

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data 1", () => {
    let result = solvePart2(sampleData1)
    let expected = 36

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 2467

    expect(result)->toEqual(expected)
  })
})
