open Jest
open Expect

//open Belt

let data = AOC2020_Day11_Data.data
let sampleData = AOC2020_Day11_Data_Sample.data

describe("2020 Day11", () => {
  open AOC2020_Day11
  let seats = sampleData->parse

  describe("SeatMap", () => {
    open SeatMap
    let getAdj_tests = list{
      (seats->getAdjacents((0, 0)), [#".", #L, #L]),
      (seats->getAdjacents((1, 0)), [#L, #L, #L, #L, #L]),
      (seats->getAdjacents((2, 0)), [#".", #L, #L, #L, #L]),
      (seats->getAdjacents((9, 0)), [#L, #L, #L]),
      (seats->getAdjacents((2, 1)), [#".", #L, #L, #L, #L, #".", #L, #"."]),
      (seats->getAdjacents((0, 9)), [#L, #".", #"."]),
      (seats->getAdjacents((8, 9)), [#L, #".", #L, #".", #L]),
      (seats->getAdjacents((9, 9)), [#".", #L, #L]),
    }

    testAll(
      "getAdjacents",
      getAdj_tests,
      ((result, expected)) => {
        expect(result)->toEqual(expected)
      },
    )
  })

  describe("next seat given a direction", () => {
    open SeatMap
    open Coordinate.StepFunctions

    let data_1 = `.......#.
                 ...#.....
                 .#.......
                 .........
                 ..#L....#
                 ....#....
                 .........
                 #........
                 ...#.....`
    let map = data_1->parse

    let init_1 = (3, 4)

    let nextSeat_test1 = list{
      (nextSeatIn(. map, init_1, stepN), #"#"),
      (nextSeatIn(. map, init_1, stepE), #"#"),
      (nextSeatIn(. map, init_1, stepS), #"#"),
      (nextSeatIn(. map, init_1, stepW), #"#"),
      (nextSeatIn(. map, init_1, stepNE), #"#"),
      (nextSeatIn(. map, init_1, stepNW), #"#"),
      (nextSeatIn(. map, init_1, stepSE), #"#"),
      (nextSeatIn(. map, init_1, stepSW), #"#"),
    }

    testAll(
      "test 1",
      nextSeat_test1,
      ((result, expected)) => {
        expect(result)->toEqual(expected)
      },
    )

    let init_2 = (3, 5)
    let nextSeat_test2 = list{
      (nextSeatIn(. map, init_2, stepN), #L),
      (nextSeatIn(. map, init_2, stepE), #"#"),
      (nextSeatIn(. map, init_2, stepS), #"#"),
      (nextSeatIn(. map, init_2, stepW), #"."),
      (nextSeatIn(. map, init_2, stepNE), #"."),
      (nextSeatIn(. map, init_2, stepNW), #"#"),
      (nextSeatIn(. map, init_2, stepSE), #"."),
      (nextSeatIn(. map, init_2, stepSW), #"."),
    }

    testAll(
      "test 2",
      nextSeat_test2,
      ((result, expected)) => {
        expect(result)->toEqual(expected)
      },
    )

    let init_3 = (0, 0)
    let nextSeat_test3 = list{
      (nextSeatIn(. map, init_3, stepN), #"."),
      (nextSeatIn(. map, init_3, stepE), #"#"),
      (nextSeatIn(. map, init_3, stepS), #"#"),
      (nextSeatIn(. map, init_3, stepW), #"."),
      (nextSeatIn(. map, init_3, stepNE), #"."),
      (nextSeatIn(. map, init_3, stepNW), #"."),
      (nextSeatIn(. map, init_3, stepSE), #"."),
      (nextSeatIn(. map, init_3, stepSW), #"."),
    }

    testAll(
      "test 3",
      nextSeat_test3,
      ((result, expected)) => {
        expect(result)->toEqual(expected)
      },
    )

    let init_4 = (4, 4)

    let nextSeat_test4 = list{
      (nextSeatIn(. map, init_4, stepN), #"."),
      (nextSeatIn(. map, init_4, stepE), #"#"),
      (nextSeatIn(. map, init_4, stepS), #"#"),
      (nextSeatIn(. map, init_4, stepW), #L),
      (nextSeatIn(. map, init_4, stepNE), #"."),
      (nextSeatIn(. map, init_4, stepNW), #"."),
      (nextSeatIn(. map, init_4, stepSE), #"."),
      (nextSeatIn(. map, init_4, stepSW), #"."),
    }

    testAll(
      "test 4",
      nextSeat_test4,
      ((result, expected)) => {
        expect(result)->toEqual(expected)
      },
    )

    let data_2 = `.............
                  .L.L.#.#.#.#.
                  .............`
    let map_2 = data_2->parse
    let nextSeat_test5 = list{
      (nextSeatIn(. map_2, (1, 1), stepE), #L),
      (nextSeatIn(. map_2, (3, 1), stepE), #"#"),
      (nextSeatIn(. map_2, (3, 1), stepW), #L),
    }

    testAll(
      "test 5",
      nextSeat_test5,
      ((result, expected)) => {
        expect(result)->toEqual(expected)
      },
    )
  })

  test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day11.solvePart1(sampleData)
    let expected = 37

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day11.solvePart1(data)
    let expected = 2270

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day11.solvePart2(data)
    let expected = 2042

    expect(result)->toEqual(expected)
  })
})
