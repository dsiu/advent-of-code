open Jest2

//open Belt
let data = AOC2022_Day8_Data.data
let sampleData = AOC2022_Day8_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2022_Day8)

describe("2022 Day8", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 21

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 1779

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 8

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 172224

    expect(result)->toEqual(expected)
  })
})
