// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Day3$AdventOfCode = require("../../../src/2018/Day3/Day3.bs.js");

Jest.describe("2018 Day3", (function (param) {
        return Jest.describe("Part1", (function (param) {
                      Jest.test("parse line", (function (param) {
                              var test_line = "#1 @ 669,271: 17x11";
                              var result = Day3$AdventOfCode.parseLine(test_line);
                              var expected = [
                                test_line,
                                "1",
                                "669",
                                "271",
                                "17",
                                "11"
                              ];
                              return Jest.Expect.toEqual(expected, Jest.Expect.expect(result));
                            }));
                      Jest.test("make claim", (function (param) {
                              var result = Day3$AdventOfCode.makeClaim("#1 @ 669,271: 17x11");
                              var expected = Day3$AdventOfCode.Claim.make(1, 669, 271, 17, 11);
                              return Jest.Expect.toEqual(expected, Jest.Expect.expect(result));
                            }));
                      Jest.test("find max x", (function (param) {
                              var result = Day3$AdventOfCode.findMaxX(Day3$AdventOfCode.allClaim([
                                        "#1 @ 100,200: 34x56",
                                        "#2 @ 200,300: 78x90"
                                      ]));
                              return Jest.Expect.toEqual(278, Jest.Expect.expect(result));
                            }));
                      return Jest.test("find max y", (function (param) {
                                    var result = Day3$AdventOfCode.findMaxY(Day3$AdventOfCode.allClaim([
                                              "#1 @ 100,200: 34x56",
                                              "#2 @ 200,300: 78x90"
                                            ]));
                                    return Jest.Expect.toEqual(390, Jest.Expect.expect(result));
                                  }));
                    }));
      }));

/*  Not a pure module */
