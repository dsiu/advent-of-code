// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Interval$AdventOfCode from "../../Interval.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function toString(t) {
  return "Rule(Src:" + Interval$AdventOfCode.toString(t.srcInterval) + ", Dest:" + t.dest.toString() + ", Offset:" + t.offset.toString() + ")";
}

function run(t, srcNum) {
  if (Interval$AdventOfCode.contains(t.srcInterval, srcNum)) {
    return srcNum + t.offset;
  }
  
}

function runWithInterval(t, src) {
  var intersection = Interval$AdventOfCode.intersect(src, t.srcInterval);
  var newSrc = Stdlib__Option.getOr(Stdlib__Option.map(intersection, (function (i) {
              return Interval$AdventOfCode.remove(src, i);
            })), src);
  var newDest = Stdlib__Option.map(intersection, (function (i) {
          return Interval$AdventOfCode.add(i, t.offset);
        }));
  return [
          newSrc,
          newDest
        ];
}

var Rule = {
  toString: toString,
  run: run,
  runWithInterval: runWithInterval
};

function toString$1(t) {
  return "AlmanacMap(" + t.srcCategory + ", " + t.destCategory + ", [" + t.rules.map(toString).join(", ") + "])";
}

function runRules(t, srcNum) {
  return Stdlib__Option.getOr(Stdlib__Array.findMap(t.rules, (function (__x) {
                    return run(__x, srcNum);
                  })), srcNum);
}

function runRulesWithInterval(t, src) {
  return Stdlib__Array.reduce(t.rules, [
              src,
              []
            ], (function (param, r) {
                var d = param[1];
                var s = param[0];
                if (s === undefined) {
                  return [
                          undefined,
                          d
                        ];
                }
                var match = runWithInterval(r, s);
                var dest = match[1];
                var newDest = dest !== undefined ? [dest].concat(d) : d;
                return [
                        match[0],
                        newDest
                      ];
              }));
}

function runRulesWithMultiIntervals(t, xs) {
  return xs.flatMap(function (x) {
              var match = runRulesWithInterval(t, x);
              var newSrc = Stdlib__Option.getOr(Stdlib__Option.flatMap(match[0], (function (s) {
                          return [s];
                        })), []);
              return newSrc.concat(match[1]);
            });
}

var AlmanacMap = {
  toString: toString$1,
  runRules: runRules,
  runRulesWithInterval: runRulesWithInterval,
  runRulesWithMultiIntervals: runRulesWithMultiIntervals
};

function toString$2(t) {
  return "Almanac (Seeds: [" + t.seeds.map(function (__x) {
                return __x.toString();
              }).join(", ") + "],\n[" + t.maps.map(toString$1).join("\n") + ")]";
}

function getMap(t, src) {
  return Stdlib__Option.getExn(Stdlib__Array.find(t.maps, (function (m) {
                    return m.srcCategory === src;
                  })), undefined);
}

var Almanac = {
  toString: toString$2,
  getMap: getMap
};

function parse(data) {
  var lines = Utils$AdventOfCode.splitDoubleNewline(data).map(function (l) {
        return Utils$AdventOfCode.splitNewline(l).map(function (prim) {
                    return prim.trim();
                  });
      });
  var seedLine = Stdlib__Option.getExn(Stdlib__Option.flatMap(lines[0], (function (__x) {
              return __x[0];
            })), undefined);
  var mapLines = (function (__x) {
        return __x.slice(1);
      })(lines);
  var parseSeed = function (line) {
    return Utils$AdventOfCode.splitSpace(Stdlib__Option.getExn(line.split(": ")[1], undefined)).map(function (prim) {
                return BigInt(prim);
              });
  };
  var parseMap = function (lines) {
    var categoryLine = Stdlib__Option.getExn(lines[0], undefined);
    var srcDestLines = lines.slice(1);
    var match = Stdlib__Option.getExn(Stdlib__Option.flatMap(Utils$AdventOfCode.splitSpace(categoryLine)[0], (function (s) {
                return (function (__x) {
                            return __x.split("-to-");
                          })(s);
              })), undefined);
    if (match.length !== 2) {
      throw {
            RE_EXN_ID: "Match_failure",
            _1: [
              "AOC2023_Day5.res",
              185,
              8
            ],
            Error: new Error()
          };
    }
    var srcCategory = match[0];
    var destCategory = match[1];
    var parseIntervalLine = function (l) {
      var match = Utils$AdventOfCode.splitSpace(l).map(function (prim) {
            return BigInt(prim);
          });
      if (match.length !== 3) {
        throw {
              RE_EXN_ID: "Match_failure",
              _1: [
                "AOC2023_Day5.res",
                193,
                10
              ],
              Error: new Error()
            };
      }
      var destStart = match[0];
      var srcStart = match[1];
      var len = match[2];
      var one = BigInt(1);
      return {
              srcInterval: Interval$AdventOfCode.make(srcStart, srcStart + len - one),
              dest: destStart,
              offset: destStart - srcStart
            };
    };
    var rules = srcDestLines.map(parseIntervalLine);
    return {
            srcCategory: srcCategory,
            destCategory: destCategory,
            rules: rules
          };
  };
  var seeds = parseSeed(seedLine);
  var maps = mapLines.map(parseMap);
  return {
          seeds: seeds,
          maps: maps
        };
}

function part1_simple(almanac) {
  var locations = almanac.seeds.map(function (s) {
        var endCat = "location";
        var _curCat = "seed";
        var _curNum = s;
        while(true) {
          var curNum = _curNum;
          var curCat = _curCat;
          var map = getMap(almanac, curCat);
          var destCategory = map.destCategory;
          var nextNum = runRules(map, curNum);
          if (destCategory === endCat) {
            return nextNum;
          }
          _curNum = nextNum;
          _curCat = destCategory;
          continue ;
        };
      });
  console.log(locations);
  return Utils$AdventOfCode.minBigIntInArray(locations);
}

function findLocation(almanac, seedTransform) {
  var newSeeds = seedTransform(almanac.seeds);
  var locations = newSeeds.flatMap(function (s) {
        var endCat = "location";
        var _curCat = "seed";
        var _cur = [s];
        while(true) {
          var cur = _cur;
          var curCat = _curCat;
          var map = getMap(almanac, curCat);
          var destCategory = map.destCategory;
          var next = runRulesWithMultiIntervals(map, cur);
          if (destCategory === endCat) {
            return next;
          }
          _cur = next;
          _curCat = destCategory;
          continue ;
        };
      });
  return Utils$AdventOfCode.minBigIntInArray(locations.map(function (param) {
                  return param[0];
                }));
}

function makeSeedsInterval(seeds) {
  return seeds.map(function (__x) {
              return Interval$AdventOfCode.makeWithLength(__x, BigInt(1));
            });
}

function makeSeedsPair(seeds) {
  return seeds.map(function (a, i) {
                  if (i % 2 !== 0) {
                    return ;
                  }
                  var b = Stdlib__Option.getExn(seeds[i + 1 | 0], undefined);
                  return Interval$AdventOfCode.makeWithLength(a, b);
                }).filter(Stdlib__Option.isSome).map(function (__x) {
              return Stdlib__Option.getExn(__x, undefined);
            });
}

function part1(__x) {
  return findLocation(__x, makeSeedsInterval);
}

function part2(__x) {
  return findLocation(__x, makeSeedsPair);
}

function solvePart1(data) {
  var almanac = parse(data);
  return findLocation(almanac, makeSeedsInterval);
}

function solvePart2(data) {
  var almanac = parse(data);
  return findLocation(almanac, makeSeedsPair);
}

export {
  log ,
  log2 ,
  Rule ,
  AlmanacMap ,
  Almanac ,
  parse ,
  part1_simple ,
  findLocation ,
  makeSeedsInterval ,
  makeSeedsPair ,
  part1 ,
  part2 ,
  solvePart1 ,
  solvePart2 ,
}
/* Stdlib__Array Not a pure module */
