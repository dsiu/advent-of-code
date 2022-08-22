open Jest
open Expect
open! Expect.Operators
open AOC2018_Day3

describe("2018 Day3", () => {
  describe("Part1", () => {
    test("parse line", () => {
      let test_line = "#1 @ 669,271: 17x11"
      let result = Claim.parseLine(test_line)
      let expected = [test_line, "1", "669", "271", "17", "11"]

      expect(result)->toEqual(expected)
    })

    test("make claim", () => {
      let test_line = "#1 @ 669,271: 17x11"
      let result = Claim.makeClaim(test_line)
      let expected = Claim.make(~id=1, ~x=669, ~y=271, ~w=17, ~h=11)

      expect(result)->toEqual(expected)
    })

    test("find max x", () => {
      let test_line1 = "#1 @ 100,200: 34x56"
      let test_line2 = "#2 @ 200,300: 78x90"

      let result = [test_line1, test_line2]->Claims.make->Claims.findMaxX

      expect(result)->toEqual(278)
    })

    test("find max y", () => {
      let test_line1 = "#1 @ 100,200: 34x56"
      let test_line2 = "#2 @ 200,300: 78x90"

      let result = [test_line1, test_line2]->Claims.make->Claims.findMaxY

      expect(result)->toEqual(390)
    })

    let add = Utils.add
    let mul = Utils.mul

    test("fabric matrix - single value per point +", () => {
      let test_fab = Fabric.make(~w=10, ~h=10)->Fabric.fill(add)
      let result1 = test_fab->Fabric.getPoint(~x=1, ~y=1)
      let result2 = test_fab->Fabric.getPoint(~x=3, ~y=5)
      expect((result1, result2))->toEqual((Some([2]), Some([8])))
    })

    test("fabric matrix - single value per point *", () => {
      let test_fab = Fabric.make(~w=10, ~h=10)->Fabric.fill(mul)
      let result1 = test_fab->Fabric.getPoint(~x=2, ~y=2)
      let result2 = test_fab->Fabric.getPoint(~x=4, ~y=6)
      expect((result1, result2))->toEqual((Some([4]), Some([24])))
    })

    test("fabric matrix - multiple value per point +/*", () => {
      let test_fab = Fabric.make(~w=15, ~h=15)->Fabric.fill(add)->Fabric.fill(mul)
      let result1 = test_fab->Fabric.getPoint(~x=9, ~y=8)
      let result2 = test_fab->Fabric.getPoint(~x=2, ~y=5)
      expect((result1, result2))->toEqual((Some([17, 72]), Some([7, 10])))
    })

    test("fabric add claim (demo case)", () => {
      let test_line1 = "#1 @ 1,3: 4x4"
      let test_line2 = "#2 @ 3,1: 4x4"
      let test_line3 = "#3 @ 5,5: 2x2"
      let allClaims = [test_line1, test_line2, test_line3]->Claims.make
      let w = allClaims->Claims.findMaxX
      let h = allClaims->Claims.findMaxY
      let test_fab = Fabric.make(~w, ~h)
      let test_fab = allClaims->Belt.Array.reduce(test_fab, (acc, i) => {
        acc->Fabric.addClaim(i)
      })

      // test_fab->Fabric.dump
      let _gp = test_fab->Fabric.getPoint
      let countOverlapTwoMore = test_fab->Fabric.countOverlap(_, Fabric.twoOrMore)
      let one = Some([1])
      let two = Some([2])
      let three = Some([3])
      // let results = (gp(~x=1, ~y=3), gp(~x=1, ~y=4), gp(~x=3, ~y=1), gp(~x=6, ~y=5), countOverlapTwoMore)
      let results = countOverlapTwoMore

      let _expected = (one, one, one, two, three, 4)

      expect(results)->toEqual(4)
    })

    test("solve Part1", () => {
      let result = solvePart1()
      expect(result)->toEqual(118223)
    })

    test("part 2 (demo case)", () => {
      let test_line1 = "#3 @ 1,3: 4x4"
      let test_line2 = "#7 @ 3,1: 4x4"
      let test_line3 = "#11 @ 5,5: 2x2"
      let allClaims = [test_line1, test_line2, test_line3]->Claims.make
      let w = allClaims->Claims.findMaxX
      let h = allClaims->Claims.findMaxY
      let test_fab = Fabric.make(~w, ~h)
      let test_fab = allClaims->Belt.Array.reduce(test_fab, (acc, i) => {
        acc->Fabric.addClaim(i)
      })
      let result = test_fab->Fabric.countNonOverlapClaim(allClaims)
      expect(result)->toEqual([Some(11)])
    })

    test("solve part2", () => {
      let result = solvePart2()
      expect(result)->toEqual([Some(412)])
    })
  })
})
