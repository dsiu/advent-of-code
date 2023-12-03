open Jest
open Expect

open Jest2
open Powerset

module Array = Belt.Array

let intCmp = (a, b) => b - a

let sortResultInt = xs => {
  xs
  ->Array.map(Belt.SortArray.Int.stableSort)
  ->Belt.SortArray.stableSortBy((a, b) => {
    Array.cmp(a, b, intCmp)
  })
}

let strCmp = (a, b) => Js.String2.localeCompare(a, b)->Belt.Float.toInt

let sortResultString = xs => {
  xs
  ->Array.map(Belt.SortArray.String.stableSort)
  ->Belt.SortArray.stableSortBy((a, b) => {
    Array.cmp(a, b, strCmp)
  })
}

describe("Powerset", () => {
  let int_array_1 = [1, 2, 3]
  let int_array_1_result = [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]->sortResultInt

  let string_array_1 = ["a", "b", "c"]
  let string_array_1_result =
    [[], ["c"], ["b"], ["b", "c"], ["a"], ["a", "c"], ["a", "b"], ["a", "b", "c"]]->sortResultString

  describe("powerset_array_with_list()", () => {
    let int_tests = [(int_array_1->powersetArrayWithList_, int_array_1_result)]
    let string_tests = [(string_array_1->powersetArrayWithList_, string_array_1_result)]

    testEach2(
      "int",
      int_tests,
      (result, expected) => {
        expect(result->sortResultInt)->toEqual(expected)
      },
    )

    testEach2(
      "string",
      string_tests,
      (result, expected) => {
        expect(result->sortResultString)->toEqual(expected)
      },
    )
  })

  describe("powerset_array()", () => {
    let int_tests = [(int_array_1->powersetArray, int_array_1_result)]
    let string_tests = [(string_array_1->powersetArray, string_array_1_result)]

    testEach2(
      "int",
      int_tests,
      (result, expected) => {
        expect(result->sortResultInt)->toEqual(expected)
      },
    )

    testEach2(
      "string",
      string_tests,
      (result, expected) => {
        expect(result->sortResultString)->toEqual(expected)
      },
    )
  })
})
