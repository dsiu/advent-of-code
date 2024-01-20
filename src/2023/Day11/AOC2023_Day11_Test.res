@@uncurried

open Jest
open Expect

//open Belt
let data = AOC2023_Day11_Data.data
let sampleData = AOC2023_Day11_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2023_Day11)

open Stdlib
open BigInt

describe("2023 Day11", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = fromInt(374)

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = fromInt(9445168)

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = fromInt(82000210)

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = fromString("742305960572")

    expect(result)->toEqual(expected)
  })
})
