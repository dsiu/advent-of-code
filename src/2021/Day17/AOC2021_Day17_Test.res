open Jest
open Expect
open Jest2

//open Belt
let data = AOC2021_Day17_Data.data
let sampleData = AOC2021_Day17_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2021_Day17)

let {parse} = module(AOC2021_Day17)
open AOC2021_Day17.TrickShot

describe("2021 Day17", () => {
  describe("launch", () => {
    let t = sampleData->parse

    let makeLaunchTests = Array.map(((v, r)) => {(launch(v, t), r)})

    let example_tests =
      [
        (
          {x: 7, y: 2},
          Hit((28, -7), [(0, 0), (7, 2), (13, 3), (18, 3), (22, 2), (25, 0), (27, -3)]),
        ),
        (
          {x: 6, y: 3},
          Hit(
            (21, -9),
            [(0, 0), (6, 3), (11, 5), (15, 6), (18, 6), (20, 5), (21, 3), (21, 0), (21, -4)],
          ),
        ),
        ({x: 9, y: 0}, Hit((30, -6), [(0, 0), (9, 0), (17, -1), (24, -3)])),
        ({x: 17, y: -4}, Miss([(0, 0), (17, -4), (33, -9)])),
        (
          {x: 6, y: 9},
          Hit(
            (21, -10),
            [
              (0, 0),
              (6, 9),
              (11, 17),
              (15, 24),
              (18, 30),
              (20, 35),
              (21, 39),
              (21, 42),
              (21, 44),
              (21, 45),
              (21, 45),
              (21, 44),
              (21, 42),
              (21, 39),
              (21, 35),
              (21, 30),
              (21, 24),
              (21, 17),
              (21, 9),
              (21, 0),
            ],
          ),
        ),
      ]->makeLaunchTests

    testEach2(
      "examples",
      example_tests,
      (result, expected) => {
        expect(result)->toEqual(expected)
      },
    )
  })

  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 45

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 5050

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 112

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 2223

    expect(result)->toEqual(expected)
  })
})
