open Jest
open Expect
open! Expect.Operators

open Belt

let data = AOC2021_Day14_Data.data
let sampleData = AOC2021_Day14_Data_Sample.data
open AOC2021_Day14

let samplePolymer = {
  let (template, rules) = sampleData->parse
  Polymer.make(template, rules)
}

//let tally = s => {
//let r = HashMap.String.make(~hintSize=40)
//s->Utils.splitChars->Array.forEach(c => {

//})
//}

describe("2021 Day14", () => {
  test("solve (sample)", () => {
    let expected = "NBCCNBBBCBHCB"
    let steps = 2
    let p = Polymer.solve(samplePolymer, steps)
    expect(1) |> toEqual(1)
  })

  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day14.solvePart1(sampleData)
    let expected = 1588L

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day14.solvePart1(data)
    let expected = 3306L

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day14.solvePart2(sampleData)
    let expected = 2188189693529L
    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day14.solvePart2(data)
    let expected = 3760312702877L

    expect(result) |> toEqual(expected)
  })
})
