open Jest2
//open Expect
//open! Expect.Operators

//open Belt
let data = AOC2021_Day15_Data.data
let sampleData = AOC2021_Day15_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2021_Day15)

module Cave = AOC2021_Day15.Cave

describe("2021 Day15", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 40

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 811

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 315

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 3012

    expect(result)->toEqual(expected)
  })
})
