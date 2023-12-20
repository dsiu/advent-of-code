// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as Interval$AdventOfCode from "../src/Interval.mjs";

Jest.describe("Interval", (function (param) {
        Jest.describe("length", (function (param) {
                Jest.test("length - returns correct length for positive interval", (function (param) {
                        var interval_0 = BigInt(1);
                        var interval_1 = BigInt(5);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var expected = BigInt(5);
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.length(interval)), expected);
                      }));
                Jest.test("length - returns correct length for zero-length interval", (function (param) {
                        var interval_0 = BigInt(3);
                        var interval_1 = BigInt(3);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var expected = BigInt(1);
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.length(interval)), expected);
                      }));
                Jest.test("length - returns correct length for negative interval", (function (param) {
                        var interval_0 = BigInt(5);
                        var interval_1 = BigInt(1);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var expected = BigInt(5);
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.length(interval)), expected);
                      }));
              }));
        Jest.describe("Sort", (function (param) {
                Jest.test("sort - sorts intervals by lower bound ascending, then upper bound ascending", (function (param) {
                        var intervals = [
                          [
                            BigInt(3),
                            BigInt(5)
                          ],
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(1),
                            BigInt(4)
                          ]
                        ];
                        var expected = [
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(1),
                            BigInt(4)
                          ],
                          [
                            BigInt(3),
                            BigInt(5)
                          ]
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.sort(intervals)), expected);
                      }));
                Jest.test("sort - handles equal lower bounds by sorting by upper bound", (function (param) {
                        var intervals = [
                          [
                            BigInt(1),
                            BigInt(5)
                          ],
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(1),
                            BigInt(4)
                          ]
                        ];
                        var expected = [
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(1),
                            BigInt(4)
                          ],
                          [
                            BigInt(1),
                            BigInt(5)
                          ]
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.sort(intervals)), expected);
                      }));
                Jest.test("sort - handles equal intervals correctly", (function (param) {
                        var intervals = [
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(1),
                            BigInt(2)
                          ]
                        ];
                        var expected = [
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(1),
                            BigInt(2)
                          ]
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.sort(intervals)), expected);
                      }));
                Jest.test("sort - handles empty array correctly", (function (param) {
                        var intervals = [];
                        var expected = [];
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.sort(intervals)), expected);
                      }));
              }));
        Jest.describe("belowNotConnected", (function (param) {
                Jest.test("belowNotConnected - intervals are below and not connected", (function (param) {
                        var interval1_0 = BigInt(1);
                        var interval1_1 = BigInt(2);
                        var interval1 = [
                          interval1_0,
                          interval1_1
                        ];
                        var interval2_0 = BigInt(4);
                        var interval2_1 = BigInt(5);
                        var interval2 = [
                          interval2_0,
                          interval2_1
                        ];
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.adjacent(interval1, interval2)), true);
                      }));
                Jest.test("belowNotConnected - intervals are connected", (function (param) {
                        var interval1_0 = BigInt(1);
                        var interval1_1 = BigInt(2);
                        var interval1 = [
                          interval1_0,
                          interval1_1
                        ];
                        var interval2_0 = BigInt(3);
                        var interval2_1 = BigInt(4);
                        var interval2 = [
                          interval2_0,
                          interval2_1
                        ];
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.adjacent(interval1, interval2)), false);
                      }));
                Jest.test("belowNotConnected - intervals are overlapping", (function (param) {
                        var interval1_0 = BigInt(1);
                        var interval1_1 = BigInt(3);
                        var interval1 = [
                          interval1_0,
                          interval1_1
                        ];
                        var interval2_0 = BigInt(2);
                        var interval2_1 = BigInt(4);
                        var interval2 = [
                          interval2_0,
                          interval2_1
                        ];
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.adjacent(interval1, interval2)), false);
                      }));
                Jest.test("belowNotConnected - intervals are above and not connected", (function (param) {
                        var interval1_0 = BigInt(4);
                        var interval1_1 = BigInt(5);
                        var interval1 = [
                          interval1_0,
                          interval1_1
                        ];
                        var interval2_0 = BigInt(1);
                        var interval2_1 = BigInt(2);
                        var interval2 = [
                          interval2_0,
                          interval2_1
                        ];
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.adjacent(interval1, interval2)), false);
                      }));
              }));
        Jest.describe("merge", (function (param) {
                Jest.test("merge - merges two overlapping intervals", (function (param) {
                        var interval1_0 = BigInt(1);
                        var interval1_1 = BigInt(3);
                        var interval1 = [
                          interval1_0,
                          interval1_1
                        ];
                        var interval2_0 = BigInt(2);
                        var interval2_1 = BigInt(4);
                        var interval2 = [
                          interval2_0,
                          interval2_1
                        ];
                        var expected_0 = BigInt(1);
                        var expected_1 = BigInt(4);
                        var expected = [
                          expected_0,
                          expected_1
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.merge(interval1, interval2)), expected);
                      }));
                Jest.test("merge - merges two connected intervals", (function (param) {
                        var interval1_0 = BigInt(1);
                        var interval1_1 = BigInt(2);
                        var interval1 = [
                          interval1_0,
                          interval1_1
                        ];
                        var interval2_0 = BigInt(3);
                        var interval2_1 = BigInt(4);
                        var interval2 = [
                          interval2_0,
                          interval2_1
                        ];
                        var expected_0 = BigInt(1);
                        var expected_1 = BigInt(4);
                        var expected = [
                          expected_0,
                          expected_1
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.merge(interval1, interval2)), expected);
                      }));
                Jest.test("merge - throws error when intervals are not connected or overlapping", (function (param) {
                        var interval1_0 = BigInt(1);
                        var interval1_1 = BigInt(2);
                        var interval1 = [
                          interval1_0,
                          interval1_1
                        ];
                        var interval2_0 = BigInt(4);
                        var interval2_1 = BigInt(5);
                        var interval2 = [
                          interval2_0,
                          interval2_1
                        ];
                        var mergeFunction = function (param) {
                          return Interval$AdventOfCode.merge(interval1, interval2);
                        };
                        return Jest.Expect.toThrow(Jest.Expect.expect(mergeFunction));
                      }));
                Jest.test("merge - interval within another interval", (function (param) {
                        var interval1_0 = BigInt(1);
                        var interval1_1 = BigInt(5);
                        var interval1 = [
                          interval1_0,
                          interval1_1
                        ];
                        var interval2_0 = BigInt(2);
                        var interval2_1 = BigInt(4);
                        var interval2 = [
                          interval2_0,
                          interval2_1
                        ];
                        var expected_0 = BigInt(1);
                        var expected_1 = BigInt(5);
                        var expected = [
                          expected_0,
                          expected_1
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.merge(interval1, interval2)), expected);
                      }));
              }));
        Jest.describe("sortAndMergeOverlaps", (function (param) {
                Jest.test("sortAndMergeOverlaps - overlapping intervals", (function (param) {
                        var intervals = [
                          [
                            BigInt(3),
                            BigInt(7)
                          ],
                          [
                            BigInt(1),
                            BigInt(5)
                          ],
                          [
                            BigInt(2),
                            BigInt(6)
                          ],
                          [
                            BigInt(4),
                            BigInt(8)
                          ]
                        ];
                        var result = Interval$AdventOfCode.sortAndMergeOverlaps(intervals);
                        var expected = [[
                            BigInt(1),
                            BigInt(8)
                          ]];
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
                Jest.test("sortAndMergeOverlaps - non-overlapping connecting intervals", (function (param) {
                        var intervals = [
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(3),
                            BigInt(4)
                          ],
                          [
                            BigInt(5),
                            BigInt(6)
                          ],
                          [
                            BigInt(7),
                            BigInt(8)
                          ]
                        ];
                        var result = Interval$AdventOfCode.sortAndMergeOverlaps(intervals);
                        var expected = [[
                            BigInt(1),
                            BigInt(8)
                          ]];
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
                Jest.test("sortAndMergeOverlaps - non-overlapping non-connecting intervals", (function (param) {
                        var intervals = [
                          [
                            BigInt(4),
                            BigInt(5)
                          ],
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(10),
                            BigInt(11)
                          ],
                          [
                            BigInt(7),
                            BigInt(8)
                          ]
                        ];
                        var result = Interval$AdventOfCode.sortAndMergeOverlaps(intervals);
                        var expected = [
                          [
                            BigInt(1),
                            BigInt(2)
                          ],
                          [
                            BigInt(4),
                            BigInt(5)
                          ],
                          [
                            BigInt(7),
                            BigInt(8)
                          ],
                          [
                            BigInt(10),
                            BigInt(11)
                          ]
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
                Jest.test("sortAndMergeOverlaps - some overlapping some connecting intervals", (function (param) {
                        var intervals = [
                          [
                            BigInt(3),
                            BigInt(5)
                          ],
                          [
                            BigInt(1),
                            BigInt(4)
                          ],
                          [
                            BigInt(9),
                            BigInt(10)
                          ],
                          [
                            BigInt(7),
                            BigInt(8)
                          ]
                        ];
                        var result = Interval$AdventOfCode.sortAndMergeOverlaps(intervals);
                        var expected = [
                          [
                            BigInt(1),
                            BigInt(5)
                          ],
                          [
                            BigInt(7),
                            BigInt(10)
                          ]
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
                Jest.test("sortAndMergeOverlaps - identical intervals", (function (param) {
                        var intervals = [
                          [
                            BigInt(1),
                            BigInt(5)
                          ],
                          [
                            BigInt(1),
                            BigInt(5)
                          ],
                          [
                            BigInt(1),
                            BigInt(5)
                          ],
                          [
                            BigInt(1),
                            BigInt(5)
                          ]
                        ];
                        var result = Interval$AdventOfCode.sortAndMergeOverlaps(intervals);
                        var expected = [[
                            BigInt(1),
                            BigInt(5)
                          ]];
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
                Jest.test("sortAndMergeOverlaps - interval within another interval", (function (param) {
                        var intervals = [
                          [
                            BigInt(1),
                            BigInt(5)
                          ],
                          [
                            BigInt(2),
                            BigInt(4)
                          ],
                          [
                            BigInt(3),
                            BigInt(3)
                          ],
                          [
                            BigInt(1),
                            BigInt(5)
                          ]
                        ];
                        var result = Interval$AdventOfCode.sortAndMergeOverlaps(intervals);
                        var expected = [[
                            BigInt(1),
                            BigInt(5)
                          ]];
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
              }));
      }));

export {
  
}
/*  Not a pure module */
