open Jest
open Expect
open! Expect.Operators
open AOC2020_Day4

open Belt

let data = AOC2020_Day4_Data.data
let testData = AOC2020_Day4_Data_Test.data

describe("2020 Day4", () => {
  test("Part 1 - Test Data", () => {
    let parsed = testData->Js.String2.split("\n\n")->Array.map(Js.String2.trim)
    let result = 1

    let expected = 1

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let parsed = data->Js.String2.split("\n")->Array.map(Js.String2.trim)
    let result = 1
    let expected = 1

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let parsed = data->Js.String2.split("\n")->Array.map(Js.String2.trim)
    let result = 1
    let expected = 1

    expect(result) |> toEqual(expected)
  })
})
