open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day13_Data.data
let sampleData = AOC2021_Day13_Data_Sample.data

let {solvePart1, solvePart2} = module(AOC2021_Day13)

describe("2021 Day13", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 17

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 710

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 16

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 97

    expect(result)->toEqual(expected)
  })
})
