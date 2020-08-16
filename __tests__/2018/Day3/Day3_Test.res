open Jest
open Expect
open! Expect.Operators
open Day3

describe("2018 Day3", () => {
   describe("Part1", () => {

    test("parse line", () => {
      let test_line = "#1 @ 669,271: 17x11"
      let result = parseLine(test_line)

      let expected = [
        test_line,
        "1",
        "669",
        "271",
        "17",
        "11"
      ]

      expect(result) |> toEqual(expected)
    })

    test("create claim", () => {
      let test_line = "#1 @ 669,271: 17x11"
      let result = createClaim(test_line)
      let expected = {id:1, x:669, y:271, w:17, h:11}
      expect(result) |> toEqual(expected)
    })

  })
})
