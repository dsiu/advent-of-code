open Jest
open Expect
open! Expect.Operators
open Day3

describe("2018 Day3", () => {
  describe("Part1", () => {
    test("parse line", () => {
      let test_line = "#1 @ 669,271: 17x11"
      let result = parseLine(test_line)
      let expected = [test_line, "1", "669", "271", "17", "11"]

      expect(result) |> toEqual(expected)
    })

    test("make claim", () => {
      let test_line = "#1 @ 669,271: 17x11"
      let result = makeClaim(test_line)
      let expected = Claim.make(~id=1, ~x=669, ~y=271, ~w=17, ~h=11)

      expect(result) |> toEqual(expected)
    })

    test("find max x", () => {
      let test_line1 = "#1 @ 100,200: 34x56"
      let test_line2 = "#2 @ 200,300: 78x90"

      let result = [test_line1, test_line2] -> allClaim -> findMaxX

      expect(result) |> toEqual(278)
    })

    test("find max y", () => {
      let test_line1 = "#1 @ 100,200: 34x56"
      let test_line2 = "#2 @ 200,300: 78x90"

      let result = [test_line1, test_line2] -> allClaim -> findMaxY

      expect(result) |> toEqual(390)
    })
  })
})
