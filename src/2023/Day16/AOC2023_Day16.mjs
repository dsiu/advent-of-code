// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Id from "rescript/lib/es6/belt_Id.js";
import * as Belt_Map from "rescript/lib/es6/belt_Map.js";
import * as Belt_Set from "rescript/lib/es6/belt_Set.js";
import * as Core__Int from "@rescript/core/src/Core__Int.mjs";
import * as PervasivesU from "rescript/lib/es6/pervasivesU.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Coord_V2$AdventOfCode from "../../Coord_V2.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function cmp(a, b) {
  return Coord_V2$AdventOfCode.compare(a, b);
}

var Position = {
  compare: Coord_V2$AdventOfCode.compare,
  cmp: cmp
};

var PositionCmp = Belt_Id.MakeComparableU({
      cmp: cmp
    });

function cmp$1(param, param$1) {
  var match = param$1._0;
  var match$1 = param._0;
  var posEq = Coord_V2$AdventOfCode.compare(match$1[0], match[0]);
  var dirEq = match$1[1] === match[1] ? 0 : 1;
  if (posEq === 0) {
    return dirEq;
  } else {
    return posEq;
  }
}

var BeamHead = {
  cmp: cmp$1
};

var BeamHeadCmp = Belt_Id.MakeComparableU(BeamHead);

function showBeamHead(param) {
  var match = param._0;
  var dirStr;
  switch (match[1]) {
    case "U" :
        dirStr = "U";
        break;
    case "D" :
        dirStr = "D";
        break;
    case "L" :
        dirStr = "L";
        break;
    case "R" :
        dirStr = "R";
        break;
    
  }
  return "(" + Coord_V2$AdventOfCode.show(match[0]) + ", " + dirStr + ")";
}

function bounds(grid) {
  var keys = Belt_Map.keysToArray(grid);
  var rows = keys.map(function (param) {
        return param[0];
      });
  var cols = keys.map(function (param) {
        return param[1];
      });
  return [
          Stdlib__Array.reduce(rows, Core__Int.Constants.minValue, (function (prim0, prim1) {
                  return Math.max(prim0, prim1);
                })),
          Stdlib__Array.reduce(cols, Core__Int.Constants.minValue, (function (prim0, prim1) {
                  return Math.max(prim0, prim1);
                }))
        ];
}

function inRange(param, b) {
  var c1 = param[1];
  var r1 = param[0];
  if (r1 >= 0 && r1 <= b[0] && c1 >= 0) {
    return c1 <= b[1];
  } else {
    return false;
  }
}

function move(pos, dir) {
  switch (dir) {
    case "U" :
        return Coord_V2$AdventOfCode.add(pos, [
                    -1,
                    0
                  ]);
    case "D" :
        return Coord_V2$AdventOfCode.add(pos, [
                    1,
                    0
                  ]);
    case "L" :
        return Coord_V2$AdventOfCode.add(pos, [
                    0,
                    -1
                  ]);
    case "R" :
        return Coord_V2$AdventOfCode.add(pos, [
                    0,
                    1
                  ]);
    
  }
}

function propagateElem(element, beamHead) {
  switch (element) {
    case "Empty" :
        var match = beamHead._0;
        var dir = match[1];
        return [{
                  TAG: "BeamHead",
                  _0: [
                    move(match[0], dir),
                    dir
                  ]
                }];
    case "SlashMirror" :
        var match$1 = beamHead._0;
        var pos = match$1[0];
        switch (match$1[1]) {
          case "U" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos, "R"),
                          "R"
                        ]
                      }];
          case "D" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos, "L"),
                          "L"
                        ]
                      }];
          case "L" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos, "D"),
                          "D"
                        ]
                      }];
          case "R" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos, "U"),
                          "U"
                        ]
                      }];
          
        }
    case "BackslashMirror" :
        var match$2 = beamHead._0;
        var pos$1 = match$2[0];
        switch (match$2[1]) {
          case "U" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos$1, "L"),
                          "L"
                        ]
                      }];
          case "D" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos$1, "R"),
                          "R"
                        ]
                      }];
          case "L" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos$1, "U"),
                          "U"
                        ]
                      }];
          case "R" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos$1, "D"),
                          "D"
                        ]
                      }];
          
        }
    case "HorizontalSplitter" :
        var match$3 = beamHead._0;
        var pos$2 = match$3[0];
        switch (match$3[1]) {
          case "U" :
          case "D" :
              break;
          case "L" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos$2, "L"),
                          "L"
                        ]
                      }];
          case "R" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos$2, "R"),
                          "R"
                        ]
                      }];
          
        }
        return [
                {
                  TAG: "BeamHead",
                  _0: [
                    move(pos$2, "L"),
                    "L"
                  ]
                },
                {
                  TAG: "BeamHead",
                  _0: [
                    move(pos$2, "R"),
                    "R"
                  ]
                }
              ];
    case "VerticalSplitter" :
        var match$4 = beamHead._0;
        var pos$3 = match$4[0];
        switch (match$4[1]) {
          case "U" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos$3, "U"),
                          "U"
                        ]
                      }];
          case "D" :
              return [{
                        TAG: "BeamHead",
                        _0: [
                          move(pos$3, "D"),
                          "D"
                        ]
                      }];
          case "L" :
          case "R" :
              break;
          
        }
        return [
                {
                  TAG: "BeamHead",
                  _0: [
                    move(pos$3, "U"),
                    "U"
                  ]
                },
                {
                  TAG: "BeamHead",
                  _0: [
                    move(pos$3, "D"),
                    "D"
                  ]
                }
              ];
    
  }
}

function propagate(grid, bounds, _energized, _beamHeads) {
  while(true) {
    var beamHeads = _beamHeads;
    var energized = _energized;
    if (beamHeads.length === 0) {
      return energized;
    }
    var bh = Stdlib__Array.headUnsafe(beamHeads);
    var bhs = Stdlib__Array.tail(beamHeads);
    if (Belt_Set.has(energized, bh)) {
      _beamHeads = bhs;
      continue ;
    }
    var $$this = Belt_Map.get(grid, bh._0[0]);
    if ($$this === undefined) {
      return PervasivesU.failwith("Beam head out of bounds");
    }
    var nexts = propagateElem($$this, bh);
    var nexts$p = nexts.filter(function (param) {
          return inRange(param._0[0], bounds);
        });
    var energized$p = Belt_Set.add(energized, bh);
    _beamHeads = bhs.concat(nexts$p);
    _energized = energized$p;
    continue ;
  };
}

function makeElement(s) {
  switch (s) {
    case "-" :
        return "HorizontalSplitter";
    case "." :
        return "Empty";
    case "/" :
        return "SlashMirror";
    case "\\" :
        return "BackslashMirror";
    case "|" :
        return "VerticalSplitter";
    default:
      return PervasivesU.failwith("Unknown element");
  }
}

function makeGrid(xss) {
  return Belt_Map.fromArray(Stdlib__Array.flatten(xss.map(function (row, rowNum) {
                      return row.map(function (col, colNum) {
                                  return [
                                          [
                                            rowNum,
                                            colNum
                                          ],
                                          makeElement(col)
                                        ];
                                });
                    })), PositionCmp);
}

function countEnergized(grid, bounds, beamHead) {
  return Stdlib__Array.uniq(Belt_Set.toArray(propagate(grid, bounds, Belt_Set.make(BeamHeadCmp), [beamHead])).map(function (param) {
                  return param._0[0];
                })).length;
}

function getEdges(grid) {
  var bounds$1 = bounds(grid);
  var maxC = bounds$1[1];
  var maxR = bounds$1[0];
  var top = Stdlib__Array.fromInitializer(maxC + 1 | 0, (function (c) {
          return {
                  TAG: "BeamHead",
                  _0: [
                    [
                      0,
                      c
                    ],
                    "D"
                  ]
                };
        }));
  var left = Stdlib__Array.fromInitializer(maxR + 1 | 0, (function (r) {
          return {
                  TAG: "BeamHead",
                  _0: [
                    [
                      r,
                      0
                    ],
                    "R"
                  ]
                };
        }));
  var right = Stdlib__Array.fromInitializer(maxR + 1 | 0, (function (r) {
          return {
                  TAG: "BeamHead",
                  _0: [
                    [
                      r,
                      maxC
                    ],
                    "L"
                  ]
                };
        }));
  var bottom = Stdlib__Array.fromInitializer(maxC + 1 | 0, (function (c) {
          return {
                  TAG: "BeamHead",
                  _0: [
                    [
                      maxR,
                      c
                    ],
                    "U"
                  ]
                };
        }));
  return Stdlib__Array.foldl1([
              top,
              bottom,
              left,
              right
            ], (function (prim0, prim1) {
                return prim0.concat(prim1);
              }));
}

function part1(xss) {
  var grid = makeGrid(xss);
  var bounds$1 = bounds(grid);
  return countEnergized(grid, bounds$1, {
              TAG: "BeamHead",
              _0: [
                [
                  0,
                  0
                ],
                "R"
              ]
            });
}

function part2(xss) {
  var grid = makeGrid(xss);
  var bounds$1 = bounds(grid);
  var edges = getEdges(grid);
  return Stdlib__Option.getExn(Stdlib__Array.maximum(edges.map(function (__x) {
                      return countEnergized(grid, bounds$1, __x);
                    }), Stdlib__Int.compare));
}

function parse(data) {
  return Utils$AdventOfCode.splitNewline(data).map(function (l) {
              return l.trim().split("");
            });
}

function solvePart1(data) {
  return part1(parse(data));
}

function solvePart2(data) {
  return part2(parse(data));
}

var $$Map;

var $$Set;

export {
  $$Map ,
  $$Set ,
  log ,
  log2 ,
  Position ,
  PositionCmp ,
  BeamHead ,
  BeamHeadCmp ,
  showBeamHead ,
  bounds ,
  inRange ,
  move ,
  propagateElem ,
  propagate ,
  makeElement ,
  makeGrid ,
  countEnergized ,
  getEdges ,
  part1 ,
  part2 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* PositionCmp Not a pure module */
