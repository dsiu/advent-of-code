// Generated by ReScript, PLEASE EDIT WITH CARE


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

export {
  north ,
  east ,
  south ,
  west ,
  northEast ,
  northWest ,
  southEast ,
  southWest ,
  stepFunc ,
  stepN ,
  stepE ,
  stepS ,
  stepW ,
  stepNE ,
  stepNW ,
  stepSE ,
  stepSW ,
}
/* No side effect */
