open Jest
open Expect
open! Expect.Operators
@@warning("-33")
open Belt
open AOC2018_Day6

let data = AOC2018_Day6_Data.data
let testData = AOC2018_Day6_Data_Sample.data

describe("2018 Day6", () => {
  describe("Part 1", () => {
    test("Parse Coord", () => {
      let td = `1,2
      3,4
      5,6`
      let expected = [Coord.make(~x=1, ~y=2), Coord.make(~x=3, ~y=4), Coord.make(~x=5, ~y=6)]
      let result = td->Js.String2.split("\n")->Coord.parseCoords

      expect(result) |> toEqual(expected)
    })

    test("maxXY", () => {
      let td = `0,2
      3,8
      5,6`
      let expected = Coord.make(~x=5, ~y=8)
      let result = td->Js.String2.split("\n")->Coord.parseCoords->Coord.maxXY

      expect(result) |> toEqual(expected)
    })

    test("minXY", () => {
      let td = `0,2
          3,8
          5,1`
      let expected = Coord.make(~x=0, ~y=1)
      let result = td->Js.String2.split("\n")->Coord.parseCoords->Coord.minXY

      expect(result) |> toEqual(expected)
    })

    test("dist", () => {
      let a = Coord.make(~x=1, ~y=1)
      let b = Coord.make(~x=14, ~y=27)
      let expected = 39
      let result = Coord.dist(a, b)

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
