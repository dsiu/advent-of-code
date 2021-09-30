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

  describe("Step Functions", () => {
    open SeatMap

    let init = (4, 4)

    let singleStep_tests = [
      (init->stepN, (4, 3)),
      (init->stepE, (5, 4)),
      (init->stepS, (4, 5)),
      (init->stepW, (3, 4)),
      (init->stepNE, (5, 3)),
      (init->stepNW, (3, 3)),
      (init->stepSE, (5, 5)),
      (init->stepSW, (3, 5)),
    ]

    testEach2("Single Step", singleStep_tests, (result, expected) => {
      expect(result)->toEqual(expected)
    })

    let multipleStep_test = [
      (init->stepN->stepN, (4, 2)),
      (init->stepE->stepE, (6, 4)),
      (init->stepS->stepW, (3, 5)),
      (init->stepNE->stepSW, (4, 4)),
      (init->stepNW->stepSE, (4, 4)),
    ]

    testEach2("Multiple Step", multipleStep_test, (result, expected) => {
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
    let expected = 2

    expect(result)->toEqual(expected)
  })
})
