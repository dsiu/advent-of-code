open Jest
open Expect
open! Expect.Operators
open Day3

describe("2018 Day3", () => {
  describe("Part1", () => {
    test("parse line", () => {
      let test_line = "#1 @ 669,271: 17x11"
      let result = Claim.parseLine(test_line)
      let expected = [test_line, "1", "669", "271", "17", "11"]

      expect(result) |> toEqual(expected)
    })

    test("make claim", () => {
      let test_line = "#1 @ 669,271: 17x11"
      let result = Claim.makeClaim(test_line)
      let expected = Claim.make(~id=1, ~x=669, ~y=271, ~w=17, ~h=11)

      expect(result) |> toEqual(expected)
    })

    test("find max x", () => {
      let test_line1 = "#1 @ 100,200: 34x56"
      let test_line2 = "#2 @ 200,300: 78x90"

      let result = [test_line1, test_line2]->Claims.make->Claims.findMaxX

      expect(result) |> toEqual(278)
    })

    test("find max y", () => {
      let test_line1 = "#1 @ 100,200: 34x56"
      let test_line2 = "#2 @ 200,300: 78x90"

      let result = [test_line1, test_line2]->Claims.make->Claims.findMaxY

      expect(result) |> toEqual(390)
    })

    let add = (x, y) => x + y
    let times = (x, y) => x * y
    test("fabric matrix - single value per point +", () => {
      let test_fab = Fabric.make(~w=10, ~h=10)->Fabric.fill(add)
      let result1 = test_fab->Fabric.getPoint(~x=1, ~y=1)
      let result2 = test_fab->Fabric.getPoint(~x=3, ~y=5)
      expect((result1, result2)) |> toEqual(([2], [8]))
    })

    test("fabric matrix - single value per point *", () => {
      let test_fab = Fabric.make(~w=10, ~h=10)->Fabric.fill(times)
      let result1 = test_fab->Fabric.getPoint(~x=2, ~y=2)
      let result2 = test_fab->Fabric.getPoint(~x=4, ~y=6)
      expect((result1, result2)) |> toEqual(([4], [24]))
    })

    test("fabric matrix - multiple value per point +/*", () => {
      let test_fab = Fabric.make(~w=15, ~h=15)->Fabric.fill(add)->Fabric.fill(times)
      let result1 = test_fab->Fabric.getPoint(~x=9, ~y=8)
      let result2 = test_fab->Fabric.getPoint(~x=2, ~y=5)
      expect((result1, result2)) |> toEqual(([17, 72], [7, 10]))
    })

    test("fabric add claim", () => {
      let test_line1 = "#1 @ 100,200: 34x56"
      let test_line2 = "#2 @ 200,300: 78x90"
      let allClaims = [test_line1, test_line2]->Claims.make
      let w = allClaims->Claims.findMaxX
      let h = allClaims->Claims.findMaxY
      let test_fab = Fabric.make(~w, ~h)
      expect(1) === 1
    })
  })
})
