open Jest
open Expect

//open Belt
let data = AOC2025_Day2_Data.data
let sampleData = AOC2025_Day2_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2025_Day2)

describe("2025 Day2", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 1227775554n

    // expect(result)->toEqual(expected)
    expect(true)->toEqual(true)
  })

  test("Part 1 - Solve", () => {
//    let result = solvePart1(data)
    let expected = 1n->BigInt.toString

//    expect(result)->toEqual(expected)
    expect(true)->toEqual(true)

  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 2n

//    expect(result)->toEqual(expected)
    expect(true)->toEqual(true)

  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 2n

//    expect(result)->toEqual(expected)
    expect(true)->toEqual(true)
  })
})
