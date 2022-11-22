// ref: https://work.njae.me.uk/2019/12/04/advent-of-code-2019-day-4/
//
// ref: https://github.com/frerich/aoc2019
//
open Jest2

//open Belt
let data = AOC2019_Day4_Data.data
let sampleData = AOC2019_Day4_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2019_Day4)

describe("2019 Day4", () => {
  //  test("Part 1 - Sample Data", () => {
  //    let result = solvePart1(sampleData)
  //    let expected = 1
  //
  //    expect(result)->toEqual(expected)
  //  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 1330

    expect(result)->toEqual(expected)
  })

  //  test("Part 2 - Sample Data", () => {
  //    let result = solvePart2(sampleData)
  //    let expected = 2
  //
  //    expect(result)->toEqual(expected)
  //  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 876

    expect(result)->toEqual(expected)
  })
})
