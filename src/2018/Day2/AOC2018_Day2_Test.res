open Jest
open Expect
open! Expect.Operators
open AOC2018_Day2

describe("2018 Day2", () => {
  let data = AOC2018_Day2_Data.data
  let lines = data->Js.String2.split("\n")
  let len = lines->Js.Array2.length

  describe("Part1", () => {
    test(
      "number of lines",
      () => {
        expect(len)->toEqual(250)
      },
    )

    test(
      "first line",
      () => {
        expect(lines[0]->Option.getExn)->toEqual("crruafyzloguvxwctqmphenbkd")
      },
    )

    test(
      "last line",
      () => {
        expect(lines[len - 1]->Option.getExn)->toEqual("hrijafyzloguvxectqmpheybkd")
      },
    )

    let test_string = "abbbcc"

    test(
      "string to char[]",
      () => {
        expect(test_string->string_to_charStr)->toEqual(["a", "b", "b", "b", "c", "c"])
      },
    )

    test(
      "char freq",
      () => {
        let expected =
          Belt.Map.String.empty
          ->Belt.Map.String.set("a", 1)
          ->Belt.Map.String.set("b", 3)
          ->Belt.Map.String.set("c", 2)

        expect(test_string->string_to_charStr->char_freq)->toEqual(expected)
      },
    )

    test(
      "char freq match",
      () => {
        expect(n_char_matched_freq(3, test_string))->toEqual(1)
      },
    )

    test(
      "threeTimesCounter",
      () => {
        let test_string = "aabbbccccccddddd"
        expect(test_string->threeTimesCounter)->toEqual(1)
      },
    )
  })

  describe("Part2", () => {
    test(
      "diffOfTwoCharStr",
      () => {
        let expected = [
          Match("a"),
          Match("b"),
          Match("c"),
          Match("d"),
          Match("e"),
          NotMatch("f", "g"),
        ]

        expect(diffOfTwoCharStr("abcdef", "abcdeg"))->toEqual(expected)
      },
    )

    test(
      "count true",
      () => {
        expect(diffOfTwoCharStr("abcdef", "abcdeg")->countTrue)->toEqual(5)
      },
    )

    test(
      "count false",
      () => {
        expect(diffOfTwoCharStr("abcdef", "abcdeg")->countTrue)->toEqual(5)
      },
    )

    test(
      "diff by 1 char strs",
      () => {
        expect(diffOfTwoCharStr("abcdef", "abcdeg")->isDiffBy1)->toBe(true)
      },
    )

    test(
      "diff by 5 char strs",
      () => {
        expect(diffOfTwoCharStr("zzzzzz", "zabcde")->isDiffBy5)->toBe(true)
      },
    )

    test(
      "run part2 test",
      () => {
        expect(["abcdef", "abcdee"]->runDay2Part2)->toEqual(["abcde", "abcde"])
      },
    )

    test(
      "run part2",
      () => {
        expect(data->Js.String2.split("\n")->runDay2Part2)->toEqual([
          "srijafjzloguvlntqmphenbkd",
          "srijafjzloguvlntqmphenbkd",
        ])
      },
    )
  })
})
