open Jest2

let data = AOC2021_Day16_Data.data
let sampleData = AOC2021_Day16_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2021_Day16)
open AOC2021_Day16
module Packet = Packet_M

describe("2021 Day16", () => {
  test("hex string to binary", () => {
    let result = hexStrToBinStr("8A004A801A8002F478")

    let expected = "110100101111111000101000"
    expect(result)->toEqual(expected)
  })
  test("parser", () => {
    let result = ""

    let expected = ""
    expect(result)->toEqual(expected)
  })

  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 1

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 1

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 2

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 2

    expect(result)->toEqual(expected)
  })
})
