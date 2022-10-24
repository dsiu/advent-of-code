open Jest2

//open Belt
let data = AOC2020_Day17_Data.data
let sampleData = AOC2020_Day17_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2020_Day17)

describe("2020 Day17", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 112

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 218

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
