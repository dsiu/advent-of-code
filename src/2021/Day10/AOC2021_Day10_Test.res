open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day10_Data.data
let sampleData = AOC2021_Day10_Data_Sample.data

describe("2021 Day10", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day10.solvePart1(sampleData)
    let expected = 26397

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day10.solvePart1(data)
    let expected = 392139

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day10.solvePart2(sampleData)
    let expected = "288957"

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day10.solvePart2(data)
    let expected = "4001832844"
    // 195150785
    expect(result) |> toEqual(expected)
  })
})
