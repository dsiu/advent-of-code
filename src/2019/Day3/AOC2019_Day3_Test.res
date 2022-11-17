open Jest2

//open Belt
let data = AOC2019_Day3_Data.data
let sampleData = AOC2019_Day3_Data_Sample.data
let sampleData1 = AOC2019_Day3_Data_Sample.data1
let sampleData2 = AOC2019_Day3_Data_Sample.data2
let {solvePart1, solvePart2} = module(AOC2019_Day3)

describe("2019 Day3", () => {
  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 6

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Sample Data 1", () => {
    let result = solvePart1(sampleData1)
    let expected = 159

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Sample Data 2", () => {
    let result = solvePart1(sampleData2)
    let expected = 135

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 855

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 30

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data 1", () => {
    let result = solvePart2(sampleData1)
    let expected = 610

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data 2", () => {
    let result = solvePart2(sampleData2)
    let expected = 410

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 11238

    expect(result)->toEqual(expected)
  })
})
