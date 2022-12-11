open Jest2

//open Belt
let data = AOC2022_Day3_Data.data
let sampleData = AOC2022_Day3_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2022_Day3)

describe("2022 Day3", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 157

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 8401

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 70

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 2

    expect(result)->toEqual(expected)
  })
})
