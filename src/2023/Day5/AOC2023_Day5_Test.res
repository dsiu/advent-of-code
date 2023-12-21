@@uncurried

open Jest
open Expect

open RescriptCore

//open Belt
let data = AOC2023_Day5_Data.data
let sampleData = AOC2023_Day5_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day5)

describe("2023 Day5", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = BigInt.fromInt(35)

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = BigInt.fromString("227653707")

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = BigInt.fromInt(46)

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = BigInt.fromString("78775051")

    expect(result)->toEqual(expected)
  })
})
