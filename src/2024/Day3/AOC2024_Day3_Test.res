open Jest
open Expect

//open Belt
let data = AOC2024_Day3_Data.data
let sampleData1 = AOC2024_Day3_Data_Sample.data1
let sampleData2 = AOC2024_Day3_Data_Sample.data2
let {solvePart1, solvePart2} = module(AOC2024_Day3)

describe("2024 Day3", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData1)
    let expected = 161

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 182780583

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData2)
    let expected = 48

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 90772405

    expect(result)->toEqual(expected)
  })
})
