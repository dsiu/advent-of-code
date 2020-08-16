open Jest;
open Expect
open! Expect.Operators
open Day1

describe("2018 Day1", () => {
  let data = Day1_Data.data
  let part2Result = Day1.runDay1Part2(data)

  test("Part 2 solve", () => {
    expect(part2Result.found) |> toBe(Some(75108))
  })
})

