open Jest
open Expect
open! Expect.Operators
open AOC2018_Day1

describe("2018 Day1", () => {
  let data = AOC2018_Day1_Data.data
  let part2Result = AOC2018_Day1.runDay1Part2(data)

  test("Part 2 solve", () => {
    expect(part2Result.found) |> toBe(Some(75108))
  })
})
