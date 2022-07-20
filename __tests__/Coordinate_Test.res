open Jest2
open Coordinate

describe("Step Functions", () => {
  let init = (4, 4)

  let singleStep_tests = [
    (stepN(. init), (4, 3)),
    (stepE(. init), (5, 4)),
    (stepS(. init), (4, 5)),
    (stepW(. init), (3, 4)),
    (stepNE(. init), (5, 3)),
    (stepNW(. init), (3, 3)),
    (stepSE(. init), (5, 5)),
    (stepSW(. init), (3, 5)),
  ]

  testEach2("Single Step", singleStep_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  let multipleStep_test = [
    (init->stepN(. _)->stepN(. _), (4, 2)),
    (init->stepE(. _)->stepE(. _), (6, 4)),
    (init->stepS(. _)->stepW(. _), (3, 5)),
    (init->stepNE(. _)->stepSW(. _), (4, 4)),
    (init->stepNW(. _)->stepSE(. _), (4, 4)),
  ]

  testEach2("Multiple Step", multipleStep_test, (result, expected) => {
    expect(result)->toEqual(expected)
  })
})
