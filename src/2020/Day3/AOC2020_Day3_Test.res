open Jest
open Expect
open! Expect.Operators
open AOC2020_Day3

open Belt

let data = AOC2020_Day3_Data.data
let testData = AOC2020_Day3_Data_Sample.data

describe("2020 Day3", () => {
  //  test("Part 1 - TreeMap make", () => {
  //    let parsed = testData->Js.String2.split("\n")->Array.map(Js.String2.trim)
  //    let result = TreeMap.make(parsed)
  //
  //    let expected0 = "..##......."->Js.String2.split("")
  //    let expected10 = ".#..#...#.#"->Js.String2.split("")
  //    expect([result[0]->Option.getExn, result[10]->Option.getExn]) |> toEqual([
  //      expected0,
  //      expected10,
  //    ])
  //  })

  test("Part 1 - Test Data", () => {
    let parsed = testData->Js.String2.split("\n")->Array.map(Js.String2.trim)
    let result = parsed->TreeMap.make->TreeMap.walk((3, 1))

    let expected = 7

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let parsed = data->Js.String2.split("\n")->Array.map(Js.String2.trim)
    let result = parsed->TreeMap.make->TreeMap.walk((3, 1))
    let expected = 162

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = "3064612320"
    expect(result) |> toBe(expected)
  })
})
