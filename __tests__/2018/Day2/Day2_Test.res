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

//  test("diff 2 char strs", () => {
//    expect(Day2.diffOfTwoCharStr("abcdef","abcdeg")) |> toEqual([true, true, true, true, true, false])
//  })

  test("diff by 1 char strs", () => {
    expect(Day2.diffOfTwoCharStr("abcdef","abcdeg")|> Day2.isDiffBy1) |> toBe(true)
  })

  test("diff by 5 char strs", () => {
      expect(Day2.diffOfTwoCharStr("zzzzzz","zabcde") |> Day2.isDiffBy5) |> toBe(true)
  })

  test("run part2 test", () => {
    expect(["abcdef","abcdee"] |> Day2.runDay2Part2) |> toBe([])
  })

})
