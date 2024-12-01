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
    let expected = BigInt.fromString("165")

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day14.solvePart1(data)
    let expected = BigInt.fromString("17765746710228")

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Mem Address Decoder 1 ", () => {
    let mask = Program.Mask.make("000000000000000000000000000000X1001X")
    let address = BigInt.fromString("42")

    let result = Program.decodeAddress(mask, address)
    let expected = ["26", "58", "27", "59"]
    expect(result)->toEqual(expected)
  })

  test("Part 2 - Mem Address Decoder 2 ", () => {
    let mask = Program.Mask.make("00000000000000000000000000000000X0XX")
    let address = BigInt.fromString("26")

    let result = Program.decodeAddress(mask, address)
    let expected = ["16", "24", "18", "26", "17", "25", "19", "27"]
    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day14.solvePart2(data)
    let expected = BigInt.fromString("4401465949086")

    expect(result)->toEqual(expected)
  })
})
