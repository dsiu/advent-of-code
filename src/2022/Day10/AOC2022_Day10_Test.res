open Jest2

//open Belt
let data = AOC2022_Day10_Data.data
let sampleData = AOC2022_Day10_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2022_Day10)

describe("2022 Day10", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 13140

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 14760

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data - Bogus - read the log output", () => {
    let result = solvePart2(sampleData)
    let expected = "EFGERURE"

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve - Bogus - read the log output", () => {
    let result = solvePart2(data)
    let expected = "EFGERURE"

    expect(result)->toEqual(expected)
  })
})
