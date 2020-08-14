open Jest;
open Expect
open! Expect.Operators

describe("2018 Day2", () => {
  let data = Day2_Data.data
  let lines = data |> Js.String.split("\n")
  let len = lines |> Array.length

  test("number of lines", () => {
    expect(len) |> toEqual(250)
  });

  test("first line", () => {
    expect(lines[0]) |> toEqual("crruafyzloguvxwctqmphenbkd")
  });

  test("last line", () => {
    expect(lines[len-1]) |> toEqual("hrijafyzloguvxectqmpheybkd")
  });

  test("string to char[]", () => {
    expect("abc" |> Day2.string_to_charStr) |> toEqual(["a", "b", "c"])
  })

})
