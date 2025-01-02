// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int from "rescript/lib/es6/Int.js";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function makeDraw(str, n) {
  switch (str) {
    case "blue" :
      return {
        TAG: "Blue",
        _0: n
      };
    case "green" :
      return {
        TAG: "Green",
        _0: n
      };
    case "red" :
      return {
        TAG: "Red",
        _0: n
      };
    default:
      return Pervasives.failwith("invalid color");
  }
}

function parse(data) {
  return Utils$AdventOfCode.splitNewline(data).map(l => {
    let ll = l.trim().split(": ");
    let id = Stdlib__Option.flatMap(ll.at(0), Utils$AdventOfCode.compose(__x => __x.replace("Game ", ""), __x => Stdlib__Int.fromString(__x, 10)));
    let draws = ll.at(1).split("; ").map(eachDraw => eachDraw.split(", ").map(singleDraw => {
      let d = singleDraw.split(" ");
      let nColor = Stdlib__Option.flatMap(d.at(0), none => Stdlib__Int.fromString(none, 10));
      let color = d.at(1);
      return makeDraw(color, nColor);
    })).flat();
    return {
      id: id,
      draws: draws
    };
  });
}

function maxNumColorsEachGame(draws) {
  return Stdlib__Array.reduce(draws, [
    {
      TAG: "Red",
      _0: 0
    },
    {
      TAG: "Green",
      _0: 0
    },
    {
      TAG: "Blue",
      _0: 0
    }
  ], (acc, x) => {
    let r = acc[0];
    switch (r.TAG) {
      case "Red" :
        let g = acc[1];
        let r$1 = r._0;
        switch (g.TAG) {
          case "Red" :
          case "Blue" :
            break;
          case "Green" :
            let b = acc[2];
            let g$1 = g._0;
            switch (b.TAG) {
              case "Blue" :
                let b$1 = b._0;
                switch (x.TAG) {
                  case "Red" :
                    return [
                      {
                        TAG: "Red",
                        _0: Primitive_int.max(x._0, r$1)
                      },
                      {
                        TAG: "Green",
                        _0: g$1
                      },
                      {
                        TAG: "Blue",
                        _0: b$1
                      }
                    ];
                  case "Blue" :
                    return [
                      {
                        TAG: "Red",
                        _0: r$1
                      },
                      {
                        TAG: "Green",
                        _0: g$1
                      },
                      {
                        TAG: "Blue",
                        _0: Primitive_int.max(x._0, b$1)
                      }
                    ];
                  case "Green" :
                    return [
                      {
                        TAG: "Red",
                        _0: r$1
                      },
                      {
                        TAG: "Green",
                        _0: Primitive_int.max(x._0, g$1)
                      },
                      {
                        TAG: "Blue",
                        _0: b$1
                      }
                    ];
                }
              case "Red" :
              case "Green" :
                break;
            }
            break;
        }
        break;
      case "Blue" :
      case "Green" :
        break;
    }
    throw {
      RE_EXN_ID: "Match_failure",
      _1: [
        "AOC2023_Day2.res",
        75,
        8
      ],
      Error: new Error()
    };
  });
}

function maxColorWithLimits(games, colorLimits) {
  return Stdlib__Array.filterMap(games, param => {
    let maxColors = maxNumColorsEachGame(param.draws);
    let r = maxColors[0];
    switch (r.TAG) {
      case "Red" :
        let g = maxColors[1];
        switch (g.TAG) {
          case "Red" :
          case "Blue" :
            break;
          case "Green" :
            let b = maxColors[2];
            switch (b.TAG) {
              case "Blue" :
                let exit = 0;
                let max_r = colorLimits[0];
                switch (max_r.TAG) {
                  case "Red" :
                    let max_g = colorLimits[1];
                    switch (max_g.TAG) {
                      case "Red" :
                      case "Blue" :
                        exit = 2;
                        break;
                      case "Green" :
                        let max_b = colorLimits[2];
                        switch (max_b.TAG) {
                          case "Blue" :
                            if (r._0 <= max_r._0 && g._0 <= max_g._0 && b._0 <= max_b._0) {
                              return [
                                param.id,
                                maxColors
                              ];
                            } else {
                              return;
                            }
                          case "Red" :
                          case "Green" :
                            exit = 2;
                            break;
                        }
                        break;
                    }
                    break;
                  case "Blue" :
                  case "Green" :
                    exit = 2;
                    break;
                }
                if (exit === 2) {
                  throw {
                    RE_EXN_ID: "Match_failure",
                    _1: [
                      "AOC2023_Day2.res",
                      107,
                      8
                    ],
                    Error: new Error()
                  };
                }
                break;
              case "Red" :
              case "Green" :
                break;
            }
            break;
        }
        break;
      case "Blue" :
      case "Green" :
        break;
    }
    throw {
      RE_EXN_ID: "Match_failure",
      _1: [
        "AOC2023_Day2.res",
        106,
        8
      ],
      Error: new Error()
    };
  });
}

function part1(games) {
  return Utils$AdventOfCode.sumIntArray(maxColorWithLimits(games, [
    {
      TAG: "Red",
      _0: 12
    },
    {
      TAG: "Green",
      _0: 13
    },
    {
      TAG: "Blue",
      _0: 14
    }
  ]).map(param => param[0]));
}

function part2(games) {
  let maxInt = Int.Constants.maxValue;
  let colorLimits_0 = {
    TAG: "Red",
    _0: maxInt
  };
  let colorLimits_1 = {
    TAG: "Green",
    _0: maxInt
  };
  let colorLimits_2 = {
    TAG: "Blue",
    _0: maxInt
  };
  let colorLimits = [
    colorLimits_0,
    colorLimits_1,
    colorLimits_2
  ];
  return Utils$AdventOfCode.sumIntArray(maxColorWithLimits(games, colorLimits).map(param => {
    let match = param[1];
    let r = match[0];
    switch (r.TAG) {
      case "Red" :
        let g = match[1];
        switch (g.TAG) {
          case "Red" :
          case "Blue" :
            break;
          case "Green" :
            let b = match[2];
            switch (b.TAG) {
              case "Blue" :
                return Math.imul(Math.imul(r._0, g._0), b._0);
              case "Red" :
              case "Green" :
                break;
            }
            break;
        }
        break;
      case "Blue" :
      case "Green" :
        break;
    }
    throw {
      RE_EXN_ID: "Match_failure",
      _1: [
        "AOC2023_Day2.res",
        123,
        14
      ],
      Error: new Error()
    };
  }));
}

function solvePart1(data) {
  return part1(parse(data));
}

function solvePart2(data) {
  return part2(parse(data));
}

export {
  log,
  makeDraw,
  parse,
  maxNumColorsEachGame,
  maxColorWithLimits,
  part1,
  part2,
  solvePart1,
  solvePart2,
}
/* Stdlib__Int Not a pure module */
