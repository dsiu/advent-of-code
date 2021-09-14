open Jest
open Belt

let data = AOC2020_Day4_Data.data
let testData = AOC2020_Day4_Data_Sample.data

describe("Array2D - make / set / get", () => {
  open Expect
  open! Expect.Operators

  test("make - int", () => {
    let a = Array2D.make((2, 3), -1)
    let _ = [
      a->Array2D.set((0, 0), 4),
      a->Array2D.set((0, 1), 5),
      a->Array2D.set((0, 2), 6),
      a->Array2D.set((1, 0), 7),
      a->Array2D.set((1, 1), 8),
      a->Array2D.set((1, 2), 9),
    ]
    let result = a
    let expected = [[4, 5, 6], [7, 8, 9]]

    expect(result) |> toEqual(expected)
  })

  test("make - string", () => {
    let a = Array2D.make((2, 3), "")
    let _ = [
      a->Array2D.set((0, 0), "a"),
      a->Array2D.set((0, 1), "b"),
      a->Array2D.set((0, 2), "c"),
      a->Array2D.set((1, 0), "d"),
      a->Array2D.set((1, 1), "e"),
      a->Array2D.set((1, 2), "f"),
    ]
    let result = a

    let expected = [["a", "b", "c"], ["d", "e", "f"]]
    //    let result = [true, true, true, true]

    expect(result) |> toEqual(expected)
  })

  test("get - string", () => {
    let a = Array2D.make((2, 2), "")
    let _ = [
      a->Array2D.set((0, 0), "e"),
      a->Array2D.set((0, 1), "f"),
      a->Array2D.set((1, 0), "g"),
      a->Array2D.set((1, 1), "h"),
    ]

    let result = [
      a->Array2D.get((0, 0))->Option.getExn,
      a->Array2D.get((0, 1))->Option.getExn,
      a->Array2D.get((1, 0))->Option.getExn,
      a->Array2D.get((1, 1))->Option.getExn,
    ]
    let expected = ["e", "f", "g", "h"]
    //    let result = [true, true, true, true]

    expect(result) |> toEqual(expected)
  })
})

describe("Array2D - keep / map / getXYEquals / crop", () => {
  let a = Array2D.make((3, 4), -1)
  let _ = [
    a->Array2D.set((0, 0), 355),
    a->Array2D.set((0, 1), 404),
    a->Array2D.set((0, 2), 320),
    a->Array2D.set((0, 3), 744),
    a->Array2D.set((1, 0), 907),
    a->Array2D.set((1, 1), 559),
    a->Array2D.set((1, 2), 891),
    a->Array2D.set((1, 3), 97),
    a->Array2D.set((2, 0), 707),
    a->Array2D.set((2, 1), 514),
    a->Array2D.set((2, 2), 982),
    a->Array2D.set((2, 3), 876),
  ]

  //  test("keep - int", () => {
  //    let result = a->Array2D.keep(x => {0 == mod(x, 2)})
  //    let expected = [[4, 6], [8]]
  //
  //    open Expect
  //    open! Expect.Operators
  //    expect(result) |> toEqual(expected)
  //  })

  test("map - int", () => {
    let result = a->Array2D.map(x => {x * 2})
    let expected = [[710, 808, 640, 1488], [1814, 1118, 1782, 194], [1414, 1028, 1964, 1752]]

    open Expect
    open! Expect.Operators
    expect(result) |> toEqual(expected)
  })

  test("getXEquals - int", () => {
    let result = a->Array2D.getXEquals(1)
    let expected = Some([907, 559, 891, 97])

    open Expect
    open! Expect.Operators
    expect(result) |> toEqual(expected)
  })

  test("getYEquals - int", () => {
    let result = [a->Array2D.getYEquals(0), a->Array2D.getYEquals(2)]
    let expected = [Some([355, 907, 707]), Some([320, 891, 982])]

    open Expect
    open! Expect.Operators
    expect(result) |> toEqual(expected)
  })

  test("corp 1 - int", () => {
    let result = a->Array2D.crop((1, 2), ~lenX=1, ~lenY=2)
    let expected = [[891, 97]]

    open Expect
    open! Expect.Operators
    expect(result) |> toEqual(expected)
  })

  test("corp 2 - int", () => {
    let result = a->Array2D.crop((1, 1), ~lenX=2, ~lenY=2)
    let expected = [[559, 891], [514, 982]]

    open Expect
    open! Expect.Operators
    expect(result) |> toEqual(expected)
  })
})
