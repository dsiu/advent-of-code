open Jest
open Expect
open! Expect.Operators

//open Belt

let data = AOC2020_Day11_Data.data
let sampleData = AOC2020_Day11_Data_Sample.data

describe("2020 Day11", () => {
  open AOC2020_Day11
  let seats = sampleData->parse

  describe("SeatMap", () => {
    test("SeatMap - getAdjacents", () => {
      open SeatMap
      let result = [
        seats->getAdjacents((0, 0)),
        seats->getAdjacents((1, 0)),
        seats->getAdjacents((2, 0)),
        seats->getAdjacents((9, 0)),
        seats->getAdjacents((2, 1)),
        seats->getAdjacents((0, 9)),
        seats->getAdjacents((8, 9)),
        seats->getAdjacents((9, 9)),
      ]
      let expected = [
        [#".", #L, #L],
        [#L, #L, #L, #L, #L],
        [#".", #L, #L, #L, #L],
        [#L, #L, #L],
        [#".", #L, #L, #L, #L, #".", #L, #"."],
        [#L, #".", #"."],
        [#L, #".", #L, #".", #L],
        [#".", #L, #L],
      ]
      expect(result) |> toEqual(expected)
    })
  })
  describe("Step Functions", () => {
    let init = (4, 4)
    test("Single Step", () => {
      open SeatMap

      let result = [
        init->stepN,
        init->stepE,
        init->stepS,
        init->stepW,
        init->stepNE,
        init->stepNW,
        init->stepSE,
        init->stepSW,
      ]
      let expected = [(4, 3), (5, 4), (4, 5), (3, 4), (5, 3), (3, 3), (5, 5), (3, 5)]

      expect(result) |> toEqual(expected)
    })

    test("Multiple Step", () => {
      open SeatMap

      let result = [
        init->stepN->stepN,
        init->stepE->stepE,
        init->stepS->stepW,
        init->stepNE->stepSW,
        init->stepNW->stepSE,
      ]
      let expected = [(4, 2), (6, 4), (3, 5), (4, 4), (4, 4)]

      expect(result) |> toEqual(expected)
    })
  })

  test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day11.solvePart1(sampleData)
    let expected = 37

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day11.solvePart1(data)
    let expected = 2270

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day11.solvePart2(data)
    let expected = 2

    expect(result) |> toEqual(expected)
  })
})
