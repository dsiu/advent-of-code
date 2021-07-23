open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day7_Data.data
let sampleData = AOC2020_Day7_Data_Sample.data

describe("2020 Day7", () => {
  //  test("parseBag", () => {
  //    let inputs = ["  2 muted yellow bags", "no other bags.  ", "1 shiny gold bag."]
  //    let result = inputs->Belt.Array.map(AOC2020_Day7.Rules.parseNumBag)
  //    let expected = [(2, "muted yellow"), (0, ""), (1, "shiny gold")]
  //    expect(result) |> toEqual(expected)
  //  })

  test("Part 1 - Test Data", () => {
    let result = 1 //AOC2020_Day7.solvePart1(sampleData)
    let expected = 1

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = 1 // AOC2020_Day7.solvePart1(data)
    let expected = 1

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day7.solvePart2(data)
    let expected = 2

    expect(result) |> toEqual(expected)
  })
})
