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
        fuse(list{"a", "b"}),
        fuse(list{"a", "A"}),
        fuse(list{"a", "B"}),
        fuse(list{"c", "C"}),
        fuse(list{"D", "d"}),
        fuse(list{"E", "Z"}),
        fuse(list{"A", "a"}),
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

    test("Solve Part 1 - testData", () => {
      let result = testCharList->defuse->List.toArray->Js.Array2.joinWith(_, "")
      // dabAcCaCBAcCcaDA
      let expected = "dabCBAcaDA"
      expect(result) |> toEqual(expected)
    })

    test("Solve Part 1 - testData fast", () => {
      let result = testCharList->defuse_fast->List.toArray->Js.Array2.joinWith(_, "")
      // dabAcCaCBAcCcaDA
      let expected = "dabCBAcaDA"
      expect(result) |> toEqual(expected)
    })

    test("Solve Part 1 - Data", () => {
      let result = charList->defuse_fast->List.toArray->Js.Array2.joinWith(_, "")
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
