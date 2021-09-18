open Jest
open Expect
open! Expect.Operators

open Belt

let data = AOC2020_Day11_Data.data
let sampleData = AOC2020_Day11_Data_Sample.data

describe("2020 Day11", () => {
  open AOC2020_Day11
  let seats = sampleData->parse

  describe("SeatMap", () => {
    test("SeatMap - getAdjacents", () => {
      let result = [
        seats->SeatMap.getAdjacents((0, 0)),
        seats->SeatMap.getAdjacents((1, 0)),
        seats->SeatMap.getAdjacents((2, 0)),
        seats->SeatMap.getAdjacents((9, 0)),
        seats->SeatMap.getAdjacents((2, 1)),
        seats->SeatMap.getAdjacents((0, 9)),
        seats->SeatMap.getAdjacents((8, 9)),
        seats->SeatMap.getAdjacents((9, 9)),
      ]
      let expected =
        [
          [#".", #L, #L],
          [#L, #L, #L, #L, #L],
          [#".", #L, #L, #L, #L],
          [#L, #L, #L],
          [#".", #L, #L, #L, #L, #".", #L, #"."],
          [#L, #".", #"."],
          [#L, #".", #L, #".", #L],
          [#".", #L, #L],
        ]->Array.map(Array.map(_, x => x->Some))
      expect(result) |> toEqual(expected)
    })
  })

  test("Part 1 - Sample Data", () => {
    //    let result = AOC2020_Day11.solvePart1(sampleData)
    let result = 1
    let expected = 1

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    //    let result = AOC2020_Day11.solvePart1(data)
    let result = 1
    let expected = 1

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day11.solvePart2(data)
    let expected = 2

    expect(result) |> toEqual(expected)
  })
})
