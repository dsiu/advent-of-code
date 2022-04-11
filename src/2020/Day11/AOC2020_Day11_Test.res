open Jest2

//open Belt

let data = AOC2020_Day11_Data.data
let sampleData = AOC2020_Day11_Data_Sample.data

describe("2020 Day11", () => {
  open AOC2020_Day11
  let seats = sampleData->parse

  describe("SeatMap", () => {
    open SeatMap
    let getAdj_tests = [
      (seats->getAdjacents((0, 0)), [#".", #L, #L]),
      (seats->getAdjacents((1, 0)), [#L, #L, #L, #L, #L]),
      (seats->getAdjacents((2, 0)), [#".", #L, #L, #L, #L]),
      (seats->getAdjacents((9, 0)), [#L, #L, #L]),
      (seats->getAdjacents((2, 1)), [#".", #L, #L, #L, #L, #".", #L, #"."]),
      (seats->getAdjacents((0, 9)), [#L, #".", #"."]),
      (seats->getAdjacents((8, 9)), [#L, #".", #L, #".", #L]),
      (seats->getAdjacents((9, 9)), [#".", #L, #L]),
    ]

    testEach2("getAdjacents", getAdj_tests, (result, expected) => {
      expect(result)->toEqual(expected)
    })
  })

  describe("next seat given a direction", () => {
    open SeatMap
    open Coordinate

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

    let nextSeat_test1 = [
      (map->nextSeatIn(init_1, stepN), #"#"),
      (map->nextSeatIn(init_1, stepE), #"#"),
      (map->nextSeatIn(init_1, stepS), #"#"),
      (map->nextSeatIn(init_1, stepW), #"#"),
      (map->nextSeatIn(init_1, stepNE), #"#"),
      (map->nextSeatIn(init_1, stepNW), #"#"),
      (map->nextSeatIn(init_1, stepSE), #"#"),
      (map->nextSeatIn(init_1, stepSW), #"#"),
    ]

    testEach2("test 1", nextSeat_test1, (result, expected) => {
      expect(result)->toEqual(expected)
    })

    let init_2 = (3, 5)
    let nextSeat_test2 = [
      (map->nextSeatIn(init_2, stepN), #L),
      (map->nextSeatIn(init_2, stepE), #"#"),
      (map->nextSeatIn(init_2, stepS), #"#"),
      (map->nextSeatIn(init_2, stepW), #"."),
      (map->nextSeatIn(init_2, stepNE), #"."),
      (map->nextSeatIn(init_2, stepNW), #"#"),
      (map->nextSeatIn(init_2, stepSE), #"."),
      (map->nextSeatIn(init_2, stepSW), #"."),
    ]

    testEach2("test 2", nextSeat_test2, (result, expected) => {
      expect(result)->toEqual(expected)
    })

    let init_3 = (0, 0)
    let nextSeat_test3 = [
      (map->nextSeatIn(init_3, stepN), #"."),
      (map->nextSeatIn(init_3, stepE), #"#"),
      (map->nextSeatIn(init_3, stepS), #"#"),
      (map->nextSeatIn(init_3, stepW), #"."),
      (map->nextSeatIn(init_3, stepNE), #"."),
      (map->nextSeatIn(init_3, stepNW), #"."),
      (map->nextSeatIn(init_3, stepSE), #"."),
      (map->nextSeatIn(init_3, stepSW), #"."),
    ]

    testEach2("test 3", nextSeat_test3, (result, expected) => {
      expect(result)->toEqual(expected)
    })

    let init_4 = (4, 4)

    let nextSeat_test4 = [
      (map->nextSeatIn(init_4, stepN), #"."),
      (map->nextSeatIn(init_4, stepE), #"#"),
      (map->nextSeatIn(init_4, stepS), #"#"),
      (map->nextSeatIn(init_4, stepW), #L),
      (map->nextSeatIn(init_4, stepNE), #"."),
      (map->nextSeatIn(init_4, stepNW), #"."),
      (map->nextSeatIn(init_4, stepSE), #"."),
      (map->nextSeatIn(init_4, stepSW), #"."),
    ]

    testEach2("test 4", nextSeat_test4, (result, expected) => {
      expect(result)->toEqual(expected)
    })

    let data_2 = `.............
                  .L.L.#.#.#.#.
                  .............`
    let map_2 = data_2->parse
    let nextSeat_test5 = [
      (map_2->nextSeatIn((1, 1), stepE), #L),
      (map_2->nextSeatIn((3, 1), stepE), #"#"),
      (map_2->nextSeatIn((3, 1), stepW), #L),
    ]

    testEach2("test 5", nextSeat_test5, (result, expected) => {
      expect(result)->toEqual(expected)
    })
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
