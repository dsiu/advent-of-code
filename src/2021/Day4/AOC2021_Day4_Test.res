open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2021_Day4_Data.data
let sampleData = AOC2021_Day4_Data_Sample.data

describe("2020 DayX", () => {
  describe("board", () => {
    test("result", () => {
      open AOC2021_Day4
      open Belt

      let b = `14 21 17 24  4
                         10 16 15  9 19
                         18  8 23 26 20
                         22 11 13  6  5
                          2  0 12  3  7`

      let board = b->Utils.splitNewline->Board.make
      let result = board->Board.solve([7, 3, 0, 12, 2])
      let expected = Some([0, 2, 3, 7, 12])

      expect(result) |> toEqual(expected)
    })
  })

  test("Part 1 - Sample Data", () => {
    //    let result = AOC2021_Day4.solvePart1(sampleData)
    let result = 1
    let expected = 1

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    //    let result = AOC2021_Day4.solvePart1(data)
    let result = 1
    let expected = 1

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2021_Day4.solvePart2(data)
    let expected = 2

    expect(result) |> toEqual(expected)
  })
})
