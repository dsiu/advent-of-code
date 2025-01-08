// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.mjs";
import * as Js_int from "rescript/lib/es6/Js_int.js";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";

function log(prim) {
  console.log(prim);
}

function velocityNext(param) {
  let x = param.x;
  return {
    x: x === 0 ? 0 : (
        x < 0 ? x + 1 | 0 : x - 1 | 0
      ),
    y: param.y - 1 | 0
  };
}

function isTargetHit_(param, param$1) {
  let y = param[1];
  let x = param[0];
  if (x >= param$1.x_min && x <= param$1.x_max && y >= param$1.y_min) {
    return y <= param$1.y_max;
  } else {
    return false;
  }
}

function isTargetHit(t_init) {
  return __x => isTargetHit_(__x, t_init);
}

function isOutOfRange_(param, param$1) {
  if (param[0] > param$1.x_max) {
    return true;
  } else {
    return param[1] < param$1.y_min;
  }
}

function isOUtOfRange(t_init) {
  return __x => isOutOfRange_(__x, t_init);
}

function toString(r) {
  let trajectory_str = __x => Belt_Array.map(__x, param => "(" + String(param[0]) + ", " + String(param[1]) + ")\n");
  if (r.TAG === "Hit") {
    let match = r._0;
    let t_str = trajectory_str(r._1);
    return "Hit: (" + String(match[0]) + ", " + String(match[1]) + ") | trajectory: [" + Utils.Printable.$$Array.toString(t_str, str => str) + "]";
  }
  let t_str$1 = trajectory_str(r._0);
  return "Miss: trajectory: [" + Utils.Printable.$$Array.toString(t_str$1, str => str) + "]";
}

function launch(v0, target) {
  let isHit = __x => isTargetHit_(__x, target);
  let isOutOfRange = __x => isOutOfRange_(__x, target);
  let _c = [
    0,
    0
  ];
  let _v = v0;
  let _trajectory = [];
  while (true) {
    let trajectory = _trajectory;
    let v = _v;
    let c = _c;
    if (isHit(c)) {
      return {
        TAG: "Hit",
        _0: c,
        _1: trajectory
      };
    }
    if (isOutOfRange(c)) {
      return {
        TAG: "Miss",
        _0: Belt_Array.concat(trajectory, [c])
      };
    }
    _trajectory = Belt_Array.concat(trajectory, [c]);
    _v = velocityNext(v);
    _c = [
      c[0] + v.x | 0,
      c[1] + v.y | 0
    ];
    continue;
  };
}

function iterate(vx_start, vx_end, vy_start, vy_end, target) {
  let result = [];
  for (let x = vx_start; x <= vx_end; ++x) {
    for (let y = vy_start; y <= vy_end; ++y) {
      let v = {
        x: x,
        y: y
      };
      let r = launch(v, target);
      if (r.TAG === "Hit") {
        result = Belt_Array.concat(result, [[
            v,
            r
          ]]);
      }
      
    }
  }
  return result;
}

function part1(vx_start, vx_end, vy_start, vy_end, target) {
  let getMaxY = __x => Belt_Array.reduce(__x, Js_int.min, (acc, c) => {
    if (c[1] > acc) {
      return c[1];
    } else {
      return acc;
    }
  });
  let launch_results = iterate(vx_start, vx_end, vy_start, vy_end, target);
  return Belt_Array.reduce(launch_results, [
    Js_int.min,
    Belt_Option.getExn(Belt_Array.get(launch_results, 0))
  ], (acc, param) => {
    let r$p = param[1];
    if (r$p.TAG !== "Hit") {
      return acc;
    }
    let y = getMaxY(r$p._1);
    if (y > acc[0]) {
      return [
        y,
        [
          param[0],
          r$p
        ]
      ];
    } else {
      return acc;
    }
  });
}

function part2(vx_start, vx_end, vy_start, vy_end, target) {
  return iterate(vx_start, vx_end, vy_start, vy_end, target).length;
}

let TrickShot = {
  velocityNext: velocityNext,
  isTargetHit_: isTargetHit_,
  isTargetHit: isTargetHit,
  isOutOfRange_: isOutOfRange_,
  isOUtOfRange: isOUtOfRange,
  toString: toString,
  launch: launch,
  iterate: iterate,
  part1: part1,
  part2: part2
};

function parse(data) {
  let match = data.replace("target area: ", "").split(", ");
  if (match.length !== 2) {
    throw {
      RE_EXN_ID: "Match_failure",
      _1: [
        "AOC2021_Day17.res",
        118,
        6
      ],
      Error: new Error()
    };
  }
  let x_str = match[0];
  let y_str = match[1];
  let match$1 = Belt_Array.map(x_str.replace("x=", "").split(".."), Utils.intFromStringExn);
  if (match$1.length !== 2) {
    throw {
      RE_EXN_ID: "Match_failure",
      _1: [
        "AOC2021_Day17.res",
        119,
        6
      ],
      Error: new Error()
    };
  }
  let x_min = match$1[0];
  let x_max = match$1[1];
  let match$2 = Belt_Array.map(y_str.replace("y=", "").split(".."), Utils.intFromStringExn);
  if (match$2.length !== 2) {
    throw {
      RE_EXN_ID: "Match_failure",
      _1: [
        "AOC2021_Day17.res",
        121,
        6
      ],
      Error: new Error()
    };
  }
  let y_min = match$2[0];
  let y_max = match$2[1];
  return {
    x_min: x_min,
    x_max: x_max,
    y_min: y_min,
    y_max: y_max
  };
}

function solvePart1(data) {
  let t = parse(data);
  return part1(1, t.x_max - 1 | 0, 0, (-t.y_min | 0) - 1 | 0, t)[0];
}

function solvePart2(data) {
  let t = parse(data);
  let vy_start = -Math.abs(t.y_min) | 0;
  let vy_end = -vy_start | 0;
  return part2(0, t.x_max, vy_start, vy_end, t);
}

export {
  log,
  TrickShot,
  parse,
  solvePart1,
  solvePart2,
}
/* Utils Not a pure module */
