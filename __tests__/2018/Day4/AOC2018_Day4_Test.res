open Jest
open Expect
open! Expect.Operators
@@warning("-33")
open AOC2018_Day4

let data = AOC2018_Day4_Data.data
let testData = AOC2018_Day4_Data_Test.data

describe("2018 Day4", () => {
  describe("Part 1", () => {
    test("Solve Part 1 - testData", () => {
      let result = AOC2018_Day4.solvePart1(testData)
      let expected = 240
      expect(result) |> toEqual(expected)
    })

    test("Solve Part 1 - Data", () => {
      let result = AOC2018_Day4.solvePart1(data)
      let expected = 48680
      expect(result) |> toEqual(expected)
    })
  })

  describe("Part 2", () => {
    test("Solve Part 2 - testData", () => {
      let result = AOC2018_Day4.solvePart2(testData)
      let expected = 4455
      expect(result) |> toEqual(expected)
    })

    test("Solve Part 2 - Data", () => {
      let result = AOC2018_Day4.solvePart2(data)
      let expected = 94826
      expect(result) |> toEqual(expected)
    })
  })
})
