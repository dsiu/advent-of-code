open Jest
open Expect

//open Belt
let data = AOC2019_Day2_Data.data
let sampleData = AOC2019_Day2_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2019_Day2)

describe("2019 Day2a", () => {
  //  test("Part 1 - Sample Data", () => {
  //    let result = solvePart1(sampleData)
  //    let expected = 1
  //
  //    expect(result)->toEqual(expected)
  //  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 4945026

    expect(result)->toEqual(expected)
  })

  //  test("Part 2 - Sample Data", () => {
  //    let result = solvePart2(sampleData)
  //    let expected = 2
  //
  //    expect(result)->toEqual(expected)
  //  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 5296

    expect(result)->toEqual(expected)
  })
})
