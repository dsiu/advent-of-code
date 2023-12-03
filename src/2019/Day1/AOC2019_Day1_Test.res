open Jest
open Expect

//open Belt
let data = AOC2019_Day1_Data.data
let sampleData = AOC2019_Day1_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2019_Day1)

describe("2019 Day1", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 34241

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 3198599

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 51316

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 4795042

    expect(result)->toEqual(expected)
  })
})
