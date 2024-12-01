// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";

function north(param) {
  return [
    param[0],
    param[1] - 1 | 0
  ];
}

function east(param) {
  return [
    param[0] + 1 | 0,
    param[1]
  ];
}

function south(param) {
  return [
    param[0],
    param[1] + 1 | 0
  ];
}

function west(param) {
  return [
    param[0] - 1 | 0,
    param[1]
  ];
}

function northEast(c) {
  return east(north(c));
}

function northWest(c) {
  return west(north(c));
}

function southEast(c) {
  return east(south(c));
}

function southWest(c) {
  return west(south(c));
}

let Direction = {
  north: north,
  east: east,
  south: south,
  west: west,
  northEast: northEast,
  northWest: northWest,
  southEast: southEast,
  southWest: southWest
};

function stepFunc(param, f) {
  return f([
    param[0],
    param[1]
  ]);
}

function stepN(c) {
  return stepFunc(c, north);
}

function stepE(c) {
  return stepFunc(c, east);
}

function stepS(c) {
  return stepFunc(c, south);
}

function stepW(c) {
  return stepFunc(c, west);
}

function stepNE(c) {
  return stepFunc(c, northEast);
}

function stepNW(c) {
  return stepFunc(c, northWest);
}

function stepSE(c) {
  return stepFunc(c, southEast);
}

function stepSW(c) {
  return stepFunc(c, southWest);
}

let StepFunctions = {
  stepFunc: stepFunc,
  stepN: stepN,
  stepE: stepE,
  stepS: stepS,
  stepW: stepW,
  stepNE: stepNE,
  stepNW: stepNW,
  stepSE: stepSE,
  stepSW: stepSW
};

function toString(param) {
  return "(" + param[0].toString() + "," + param[1].toString() + ")";
}

function xy(param, param$1) {
  let c = Primitive_int.compare(param[0], param$1[0]);
  if (c === 0) {
    return Primitive_int.compare(param[1], param$1[1]);
  } else {
    return c;
  }
}

function yx(param, param$1) {
  let c = Primitive_int.compare(param[1], param$1[1]);
  if (c === 0) {
    return Primitive_int.compare(param[0], param$1[0]);
  } else {
    return c;
  }
}

let Compare = {
  xy: xy,
  yx: yx
};

export {
  Direction,
  StepFunctions,
  toString,
  Compare,
}
/* No side effect */
