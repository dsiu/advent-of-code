open Jest
open Expect
open! Expect.Operators
open Day4

let data = Day4_Data.data
let testData = Day4_Data_Test.data

describe("2018 Day4", () => {
  describe("Part 1", () => {
    test("Solve Part 1 - testData", () => {
      let result = Day4.solvePart1(testData)
      let expected = 240
      expect(result) |> toEqual(expected)
    })

    test("Solve Part 1 - Data", () => {
      let result = Day4.solvePart1(data)
      let expected = 48680
      expect(result) |> toEqual(expected)
    })
  })

  describe("Part 2", () => {
    test("Solve Part 2 - testData", () => {
      let result = Day4.solvePart2(testData)
      let expected = 4455
      expect(result) |> toEqual(expected)
    })

    test("Solve Part 2 - Data", () => {
      let result = Day4.solvePart2(data)
      let expected = 94826
      expect(result) |> toEqual(expected)
    })
  })
})
