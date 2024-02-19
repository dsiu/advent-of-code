open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day13_Data.data
let sampleData = AOC2020_Day13_Data_Sample.data

describe("2020 Day13", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day13.solvePart1(sampleData)
    let expected = 295

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day13.solvePart1(data)
    let expected = 104

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day13.solvePart2(data)
    let expected = "842186186521918"->Stdlib.BigInt.fromString

    expect(result)->toEqual(expected)
  })
})
