open Jest2
open Coordinate

describe("Step Functions", () => {
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
