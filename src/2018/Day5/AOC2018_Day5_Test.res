open Jest
open Expect
open! Expect.Operators
open Belt
open AOC2018_Day5

let data = AOC2018_Day5_Data.data
let sampleData = AOC2018_Day5_Data_Sample.data
// dabAcCaCBAcCcaDA

let testCharArray = sampleData->Js.String2.split("")
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

    test("Solve Part 1 - sampleData list", () => {
      let result = testCharList->defuse->List.toArray->Js.Array2.joinWith(_, "")
      // dabAcCaCBAcCcaDA
      let expected = "dabCBAcaDA"
      expect(result) |> toEqual(expected)
    })

    test("Solve Part 1 - sampleData array", () => {
      let result = testCharArray->defuse_array->Js.Array2.joinWith(_, "")
      // dabAcCaCBAcCcaDA
      let expected = "dabCBAcaDA"
      expect(result) |> toEqual(expected)
    })

    // answer: 11814
    // skipped by default since it takes 20 sec
    Skip.test("Solve Part 1 - Data array", () => {
      let result = charArray->defuse_array->Js.Array2.joinWith(_, "")->Js.String.length

      let expected = AOC2018_Day5_Data.result->Js.String.length
      expect(result) |> toEqual(expected)
    })
  })

  describe("Part 2", () => {
    test("notIsLetterAndUpper", () => {
      let result = (
        testCharArray->Array.keep(notIsLetterAndUpper("a")),
        testCharArray->Array.keep(notIsLetterAndUpper("b")),
      )
      let expected = ("dbcCCBcCcD"->Js.String2.split(""), "daAcCaCAcCcaDA"->Js.String2.split(""))
      expect(result) |> toEqual(expected)
    })

    test("Solve Part 2 - sampleData", () => {
      let result = AOC2018_Day5.solvePart2(aTod, testCharArray)
      let expected = 4
      expect(result) |> toEqual(expected)
    })

    // skipped by default since it takes 20sec x 26
    Skip.test("Solve Part 2 - Data", () => {
      let result = AOC2018_Day5.solvePart2(aToz, charArray)
      let expected = 4282
      expect(result) |> toEqual(expected)
    })
  })
})
