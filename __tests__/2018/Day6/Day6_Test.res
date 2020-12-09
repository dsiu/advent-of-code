open Jest
open Expect
open! Expect.Operators
@@warning("-33")
open Belt
open Day6

let data = Day6_Data.data
let testData = Day6_Data_Test.data

describe("2018 Day6", () => {
  describe("Part 1", () => {

    test("Parse Coord", () => {
      let td = `1,2
      3,4
      5,6`
      let result = [Coord.make(~x=1,~y=2),Coord.make(~x=3,~y=4),Coord.make(~x=5,~y=6)]
      let expected = td->Js.String2.split("\n")->Coord.parseCoords

      expect(result) |> toEqual(expected)
    })

    Skip.test("Solve Part 1", () => {
      let result = 1

      let expected = 1
      expect(result) |> toEqual(expected)
    })
  })

  describe("Part 2", () => {

    Skip.test("Solve Part 2", () => {
      let result = 1

      let expected = 1
      expect(result) |> toEqual(expected)
    })
  })


})
