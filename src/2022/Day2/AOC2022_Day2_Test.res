open Jest2

//open Belt
let data = AOC2022_Day2_Data.data
let sampleData = AOC2022_Day2_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2022_Day2)

describe("2022 Day2", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 15

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 11603

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 12

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 12725

    expect(result)->toEqual(expected)
  })
})
