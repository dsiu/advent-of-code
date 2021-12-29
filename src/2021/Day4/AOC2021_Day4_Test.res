open Jest2
//open Belt

let data = AOC2021_Day4_Data.data
let sampleData = AOC2021_Day4_Data_Sample.data

describe("2020 Day4", () => {
  describe("board", () => {
    open AOC2021_Day4
    let b = `14 21 17 24  4
             10 16 15  9 19
             18  8 23 26 20
             22 11 13  6  5
             2  0 12  3  7`
    let board = b->Utils.splitNewline->Board.make

    let board_tests = [
      (
        board->Board.solve([7, 3, 0, 12, 2]),
        Some([4, 5, 6, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26]),
      ),
      (
        board->Board.solve([99, 7, 3, 0, 2, 12, 13]),
        Some([4, 5, 6, 8, 9, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26]),
      ),
      (
        board->Board.solve([24, 9, 6, 3, 26]),
        Some([0, 2, 4, 5, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]),
      ),
      (
        board->Board.solve([24, 9, 12, 6, 3, 26]),
        Some([0, 2, 4, 5, 7, 8, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]),
      ),
    ]

    testEach2("board tests", board_tests, (result, expected) => {
      expect(result)->toEqual(expected)
    })
  })

  test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day4.solvePart1(sampleData)
    let expected = 4512

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2021_Day4.solvePart1(data)
    let expected = 41503

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day4.solvePart2(data)
    let expected = 2

    expect(result)->toEqual(expected)
  })
})
