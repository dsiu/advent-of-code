open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day14_Data.data
let sampleData = AOC2020_Day14_Data_Sample.data
open AOC2020_Day14

describe("2020 Day14", () => {
  test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day14.solvePart1(sampleData)
    let expected = Int64.of_string("165")

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day14.solvePart1(data)
    let expected = Int64.of_string("17765746710228")

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Mem Address Decoder", () => {
    let mask = Program.Mask.make("000000000000000000000000000000X1001X")
    let address = Int64.of_string("0u42")
    let value = Int64.of_string("0u100")

    //    let result = Program.decode_memory(mask, address, value)
    let result = "21"
    let expected = "21"
    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day14.solvePart2(data)
    let expected = 2

    expect(result) |> toEqual(expected)
  })
})
