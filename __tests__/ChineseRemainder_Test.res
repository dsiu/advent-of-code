open Jest
open Expect
open Jest2

open ChineseRemainder

describe("ChineseRemainder", () => {
  let int_tests = [
    (crt([3, 4], [2, 5]), 9),
    (crt([2, 4], [4, 5]), 14),
    (crt([2000, 300], [2, 33]), 36),
    (crt([2, 12, 22], [13, 3, 2]), 54),
    (crt([44, 12, 10], [2, 5, 3]), 22),
  ]
  test("", () => true->expect->toEqual(true))

  testEach2("int", int_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  open RescriptCore
  let bigint_tests = [
    (
      crtBigInt([3->BigInt.fromInt, 4->BigInt.fromInt], [2->BigInt.fromInt, 5->BigInt.fromInt]),
      9->BigInt.fromInt,
    ),
    (
      crtBigInt(
        [
          BigInt.fromString("507483274265132509471575639764027"),
          BigInt.fromString("27723967616827289286920296659419136"),
        ],
        [
          BigInt.fromString("269916455047188404153874847098609926219"),
          BigInt.fromString("170141183460469231731687303715884105728"),
        ],
      ),
      BigInt.fromString(
        "26140225850797799686267310622940276042186008390341917470797088078891078123520",
      ),
    ),
  ]

  testEach2("big int", bigint_tests, (result, expected) => {
    expect(result->BigInt.toString)->toEqual(expected->BigInt.toString)
  })
})
