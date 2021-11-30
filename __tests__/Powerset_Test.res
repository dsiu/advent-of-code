open Jest2
open Powerset

describe("Powerset", () => {
  let int_array_1 = [1, 2, 3]
  let int_array_1_result = [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]
  let string_array_1 = ["a", "b", "c"]
  let string_array_1_result = [
    [],
    ["c"],
    ["b"],
    ["b", "c"],
    ["a"],
    ["a", "c"],
    ["a", "b"],
    ["a", "b", "c"],
  ]

  describe("powerset_array_with_list()", () => {
    let int_tests = [(int_array_1->powerset_array_with_list, int_array_1_result)]
    let string_tests = [(string_array_1->powerset_array_with_list, string_array_1_result)]

    testEach2("int", int_tests, (result, expected) => {
      expect(result)->toEqual(expected)
    })

    testEach2("string", string_tests, (result, expected) => {
      expect(result)->toEqual(expected)
    })
  })

  describe("powerset_array()", () => {
    let int_tests = [(int_array_1->powerset_array, int_array_1_result)]
    let string_tests = [(string_array_1->powerset_array, string_array_1_result)]

    testEach2("int", int_tests, (result, expected) => {
      expect(result)->toEqual(expected)
    })

    testEach2("string", string_tests, (result, expected) => {
      expect(result)->toEqual(expected)
    })
  })
})
