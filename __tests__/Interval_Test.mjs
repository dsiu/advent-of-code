// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as Interval$AdventOfCode from "../src/Interval.mjs";

Jest.describe("Interval", (function () {
        Jest.describe("length", (function () {
                Jest.test("length - returns correct length for positive interval", (function () {
                        var interval_0 = BigInt(1);
                        var interval_1 = BigInt(5);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var expected = BigInt(5);
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.length(interval)), expected);
                      }));
                Jest.test("length - returns correct length for zero-length interval", (function () {
                        var interval_0 = BigInt(3);
                        var interval_1 = BigInt(3);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var expected = BigInt(1);
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.length(interval)), expected);
                      }));
                Jest.test("length - returns correct length for negative interval", (function () {
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
        Jest.describe("contains", (function () {
                Jest.test("contains - returns true when number is within the interval", (function () {
                        var interval_0 = BigInt(5);
                        var interval_1 = BigInt(10);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var num = BigInt(7);
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.contains(interval, num)), true);
                      }));
                Jest.test("contains - returns true when number is equal to the lower bound", (function () {
                        var interval_0 = BigInt(5);
                        var interval_1 = BigInt(10);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var num = BigInt(5);
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.contains(interval, num)), true);
                      }));
                Jest.test("contains - returns true when number is equal to the upper bound", (function () {
                        var interval_0 = BigInt(5);
                        var interval_1 = BigInt(10);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var num = BigInt(10);
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.contains(interval, num)), true);
                      }));
                Jest.test("contains - returns false when number is less than the lower bound", (function () {
                        var interval_0 = BigInt(5);
                        var interval_1 = BigInt(10);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var num = BigInt(4);
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.contains(interval, num)), false);
                      }));
                Jest.test("contains - returns false when number is greater than the upper bound", (function () {
                        var interval_0 = BigInt(5);
                        var interval_1 = BigInt(10);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var num = BigInt(11);
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.contains(interval, num)), false);
                      }));
                Jest.test("contains - returns false when the number is not in interval with length 1", (function () {
                        var interval_0 = BigInt(5);
                        var interval_1 = BigInt(5);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var num = BigInt(4);
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.contains(interval, num)), false);
                      }));
                Jest.test("contains - returns true when the number is in interval with length 1", (function () {
                        var interval_0 = BigInt(5);
                        var interval_1 = BigInt(5);
                        var interval = [
                          interval_0,
                          interval_1
                        ];
                        var num = BigInt(5);
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.contains(interval, num)), true);
                      }));
              }));
        Jest.describe("isOverlap", (function () {
                Jest.test("isOverlap - returns true when intervals overlap", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(7), BigInt(12));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.isOverlap(interval1, interval2)), true);
                      }));
                Jest.test("isOverlap - returns false when intervals do not overlap", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(11), BigInt(15));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.isOverlap(interval1, interval2)), false);
                      }));
                Jest.test("isOverlap - returns true when intervals touch at one point on right", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(10), BigInt(15));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.isOverlap(interval1, interval2)), true);
                      }));
                Jest.test("isOverlap - returns true when intervals touch at one point on left", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(6), BigInt(11));
                        var interval2 = Interval$AdventOfCode.make(BigInt(3), BigInt(6));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.isOverlap(interval1, interval2)), true);
                      }));
                Jest.test("isOverlap - returns true when a contains b", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(1), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(3), BigInt(6));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.isOverlap(interval1, interval2)), true);
                      }));
                Jest.test("isOverlap - returns true when b contains a", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(9));
                        var interval2 = Interval$AdventOfCode.make(BigInt(1), BigInt(12));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.isOverlap(interval1, interval2)), true);
                      }));
                Jest.test("isOverlap - returns true when both a and b are some", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(13), BigInt(13));
                        var interval2 = Interval$AdventOfCode.make(BigInt(13), BigInt(13));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.isOverlap(interval1, interval2)), true);
                      }));
              }));
        Jest.describe("intersect", (function () {
                Jest.test("intersect - returns intersection when intervals overlap", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(7), BigInt(12));
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.intersect(interval1, interval2)), Interval$AdventOfCode.make(BigInt(7), BigInt(10)));
                      }));
                Jest.test("intersect - returns None when intervals do not overlap", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(11), BigInt(15));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.intersect(interval1, interval2)), undefined);
                      }));
                Jest.test("intersect - returns intersection when intervals touch at one point", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(10), BigInt(15));
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.intersect(interval1, interval2)), Interval$AdventOfCode.make(BigInt(10), BigInt(10)));
                      }));
                Jest.test("intersect - returns intersection both intervals are length 1", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(5));
                        var interval2 = Interval$AdventOfCode.make(BigInt(5), BigInt(5));
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.intersect(interval1, interval2)), Interval$AdventOfCode.make(BigInt(5), BigInt(5)));
                      }));
                Jest.test("intersect - returns intersection a interval is length 1 and the other is inside", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(7), BigInt(7));
                        var interval2 = Interval$AdventOfCode.make(BigInt(1), BigInt(10));
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.intersect(interval1, interval2)), Interval$AdventOfCode.make(BigInt(7), BigInt(7)));
                      }));
              }));
        Jest.describe("below", (function () {
                Jest.test("below - returns true when first interval is below the second", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(12), BigInt(15));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.below(interval1, interval2)), true);
                      }));
                Jest.test("below - returns false when first interval is above the second", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(12), BigInt(15));
                        var interval2 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.below(interval1, interval2)), false);
                      }));
                Jest.test("below - returns false when intervals overlap", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(7), BigInt(12));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.below(interval1, interval2)), false);
                      }));
              }));
        Jest.describe("adjacent", (function () {
                Jest.test("adjacent - returns true when intervals are adjacent and not overlapping", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(11), BigInt(15));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.adjacent(interval1, interval2)), true);
                      }));
                Jest.test("adjacent - returns false when intervals are overlapping", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(7), BigInt(12));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.adjacent(interval1, interval2)), false);
                      }));
                Jest.test("adjacent - returns false when intervals are not adjacent and not overlapping", (function () {
                        var interval1 = Interval$AdventOfCode.make(BigInt(5), BigInt(10));
                        var interval2 = Interval$AdventOfCode.make(BigInt(15), BigInt(20));
                        return Jest.Expect.toBe(Jest.Expect.expect(Interval$AdventOfCode.adjacent(interval1, interval2)), false);
                      }));
              }));
        Jest.describe("remove", (function () {
                Jest.test("remove - returns a when intervals do not overlap", (function () {
                        var a = Interval$AdventOfCode.make(BigInt(1), BigInt(5));
                        var b = Interval$AdventOfCode.make(BigInt(6), BigInt(10));
                        var result = Interval$AdventOfCode.remove(a, b);
                        var expected = [
                          BigInt(1),
                          BigInt(5)
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
                Jest.test("remove - returns None when b is all contained within a", (function () {
                        var a = Interval$AdventOfCode.make(BigInt(1), BigInt(10));
                        var b = Interval$AdventOfCode.make(BigInt(3), BigInt(7));
                        var result = Interval$AdventOfCode.remove(a, b);
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), undefined);
                      }));
                Jest.test("remove - returns None when a is contained within b", (function () {
                        var a = Interval$AdventOfCode.make(BigInt(3), BigInt(7));
                        var b = Interval$AdventOfCode.make(BigInt(1), BigInt(10));
                        var result = Interval$AdventOfCode.remove(a, b);
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), undefined);
                      }));
                Jest.test("remove - returns None when a and b are the same", (function () {
                        var a = Interval$AdventOfCode.make(BigInt(3), BigInt(7));
                        var b = Interval$AdventOfCode.make(BigInt(3), BigInt(7));
                        var result = Interval$AdventOfCode.remove(a, b);
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), undefined);
                      }));
                Jest.test("remove - returns None when a and b are single point intervals and are the same", (function () {
                        var a = Interval$AdventOfCode.make(BigInt(3), BigInt(3));
                        var b = Interval$AdventOfCode.make(BigInt(3), BigInt(3));
                        var result = Interval$AdventOfCode.remove(a, b);
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), undefined);
                      }));
                Jest.test("remove - returns Some(a) when a and b are single point intervals and are different", (function () {
                        var a = Interval$AdventOfCode.make(BigInt(3), BigInt(3));
                        var b = Interval$AdventOfCode.make(BigInt(4), BigInt(4));
                        var result = Interval$AdventOfCode.remove(a, b);
                        var expected = [
                          BigInt(3),
                          BigInt(3)
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
              }));
        Jest.describe("Sort", (function () {
                Jest.test("sort - sorts intervals by lower bound ascending, then upper bound ascending", (function () {
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
                Jest.test("sort - handles equal lower bounds by sorting by upper bound", (function () {
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
                Jest.test("sort - handles equal intervals correctly", (function () {
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
                Jest.test("sort - handles empty array correctly", (function () {
                        var intervals = [];
                        var expected = [];
                        return Jest.Expect.toEqual(Jest.Expect.expect(Interval$AdventOfCode.sort(intervals)), expected);
                      }));
              }));
        Jest.describe("merge", (function () {
                Jest.test("merge - merges two overlapping intervals", (function () {
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
                Jest.test("merge - merges two connected intervals", (function () {
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
                Jest.test("merge - throws error when intervals are not connected or overlapping", (function () {
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
                        var mergeFunction = function () {
                          return Interval$AdventOfCode.merge(interval1, interval2);
                        };
                        return Jest.Expect.toThrow(Jest.Expect.expect(mergeFunction));
                      }));
                Jest.test("merge - interval within another interval", (function () {
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
        Jest.describe("sortAndMergeOverlaps", (function () {
                Jest.test("sortAndMergeOverlaps - overlapping intervals", (function () {
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
                Jest.test("sortAndMergeOverlaps - non-overlapping connecting intervals", (function () {
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
                Jest.test("sortAndMergeOverlaps - non-overlapping non-connecting intervals", (function () {
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
                Jest.test("sortAndMergeOverlaps - some overlapping some connecting intervals", (function () {
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
                Jest.test("sortAndMergeOverlaps - identical intervals", (function () {
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
                Jest.test("sortAndMergeOverlaps - interval within another interval", (function () {
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
