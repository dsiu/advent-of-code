open Jest
open Expect
open! Expect.Operators

//open Belt
open AOC2020_Day7

let data = AOC2020_Day7_Data.data
let sampleData = AOC2020_Day7_Data_Sample.data

describe("2020 Day7", () => {
  test("parseBag", () => {
    let inputs = ["  2 muted yellow bags", "no other bags.  ", "1 shiny gold bag."]
    let result = inputs->Belt.Array.map(AOC2020_Day7.Rules.parseNumBag)
    let expected = [Bag.make(2, "muted yellow"), Bag.make(0, ""), Bag.make(1, "shiny gold")]
    expect(result)->toEqual(expected)
  })

  test("Part 1 - Test Data", () => {
    let result = AOC2020_Day7.solvePart1(sampleData)
    let expected = 4

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day7.solvePart1(data)
    let expected = 115

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day7.solvePart2(data)
    let expected = 1250

    expect(result)->toEqual(expected)
  })
})
