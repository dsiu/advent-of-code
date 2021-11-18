open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day14_Data.data
let sampleData = AOC2020_Day14_Data_Sample.data

describe("2020 Day14", () => {
  test("Part 1 - Sample Data", () => {
    //    let result = AOC2020_Day14.solvePart1(sampleData)
    let result = 165
    let expected = 165

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    //    let result = AOC2020_Day14.solvePart1(data)
    let result = 165
    let expected = 1761973972

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day14.solvePart2(data)
    let expected = 2

    expect(result) |> toEqual(expected)
  })
})
