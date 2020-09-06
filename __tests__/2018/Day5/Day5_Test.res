open Jest
open Expect
open! Expect.Operators
open Belt
open Day5

let data = Day5_Data.data
let testData = Day5_Data_Test.data
// dabAcCaCBAcCcaDA

let testCharArray = testData->Js.String2.split("")
let testCharList = testCharArray->List.fromArray

let charArray = data->Js.String2.split("")
let charList = charArray->List.fromArray

describe("2018 Day5", () => {
  describe("Part 1", () => {
    test("fuse", () => {
      let result = (
        fuse(("a", "b")),
        fuse(("a", "A")),
        fuse(("a", "B")),
        fuse(("c", "C")),
        fuse(("D", "d")),
        fuse(("E", "Z")),
        fuse(("A", "a")),
      )
      let expected = (false, true, false, true, true, false, true)
      expect(result) |> toEqual(expected)
    })

    // test("defuse", () => {
    // let d = list{list{"a", "b"}, list{"a", "B"}, list{"c", "C"}, list{"D", "d"}, list{"E", "Z"}}
    // let result = d->defuse_old
    // let expected = list{list{"a", "b"}, list{"a", "B"}, list{"E", "Z"}}
    //
    // expect(result) |> toEqual(expected)
    // })

    test("Solve Part 1 - testData list", () => {
      let result = testCharList->defuse->List.toArray->Js.Array2.joinWith(_, "")
      // dabAcCaCBAcCcaDA
      let expected = "dabCBAcaDA"
      expect(result) |> toEqual(expected)
    })

    test("Solve Part 1 - testData array", () => {
      let result = testCharArray->defuse_array->Js.Array2.joinWith(_, "")
      // dabAcCaCBAcCcaDA
      let expected = "dabCBAcaDA"
      expect(result) |> toEqual(expected)
    })

    // test("Solve Part 1 - Data list", () => {
    // let result = charList->defuse->List.toArray->Js.Array2.joinWith(_, "")
    // // dabAcCaCBAcCcaDA
    // let expected = Day5_Data.result
    // expect(result) |> toEqual(expected)
    // })
    //
    test("Solve Part 1 - Data array", () => {
      let result = charArray->defuse_array->Js.Array2.joinWith(_, "")
      // dabAcCaCBAcCcaDA
      let expected = Day5_Data.result
      expect(result) |> toEqual(expected)
    })
  })

  describe("Part 2", () => {
    test("Solve Part 2 - testData", () => {
      let result = Day5.solvePart2(testData)
      let expected = 4455
      expect(result) |> toEqual(expected)
    })

    test("Solve Part 2 - Data", () => {
      let result = Day5.solvePart2(data)
      let expected = 4455
      expect(result) |> toEqual(expected)
    })
  })
})
