open Jest
open Expect

//open Belt
let data = AOC2025_Day1_Data.data
let sampleData = AOC2025_Day1_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2025_Day1)

describe("2025 Day1", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 3

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 1084

    expect(result)->toEqual(expected)
  })

  testAll("Part 2 - Test Data", list{
    ("L75", 1),
  }, ((data,out)) => {
    let result = solvePart2(data)
    let expected = out

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 6

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 6475

    expect(result)->toEqual(expected)
  })
})
