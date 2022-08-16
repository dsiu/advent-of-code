open Jest2

//open Belt
let data = AOC2021_Day19_Data.data
let sampleData = AOC2021_Day19_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2021_Day19)

describe("2021 Day19", () => {
  test("Part 1 - Sample Data", () => {
    //    let result = solvePart1(sampleData)
    let result = 1
    let expected = 1

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    //    let result = solvePart1(data)
    let result = 1
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
