// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_MapString from "rescript/lib/es6/belt_MapString.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function rangeToString(param) {
  return String(param._0) + "-" + String(param._1);
}

function bodyToString(param) {
  return rangeToString(param._0) + " or " + rangeToString(param._1);
}

function inRange(param, value) {
  if (param._0 <= value) {
    return value <= param._1;
  } else {
    return false;
  }
}

function matchesRule(param, value) {
  if (inRange(param._0, value)) {
    return true;
  } else {
    return inRange(param._1, value);
  }
}

function validForAnyField(rules, value) {
  return Belt_Array.some(Belt_MapString.valuesToArray(rules._0), (function (__x) {
                return matchesRule(__x, value);
              }));
}

function ticketErrorRate(rules, tickets) {
  return Utils$AdventOfCode.sumIntArray(Utils$AdventOfCode.flatten(Belt_Array.map(tickets, (function (t) {
                        return Belt_Array.keep(t, (function (v) {
                                      return !validForAnyField(rules, v);
                                    }));
                      }))));
}

function isValidTicket(rules, ticket) {
  var rules$1 = rules._0;
  return Belt_Array.every(Belt_Array.map(ticket, (function (__x) {
                    return validForAnyField({
                                TAG: "RuleSet",
                                _0: rules$1
                              }, __x);
                  })), Utils$AdventOfCode.identity);
}

function possibleColumns(ticketCols, body) {
  var columnMatches = function (param) {
    return Belt_Array.every(param[1], (function (__x) {
                  return matchesRule(body, __x);
                }));
  };
  var idx = Belt_Array.makeBy(ticketCols.length, Utils$AdventOfCode.identity);
  return Belt_Array.map(Belt_Array.keep(Belt_Array.zip(idx, ticketCols), columnMatches), (function (prim) {
                return prim[0];
              }));
}

function possibleColumnsAll(rules, tickets) {
  var rules$1 = rules._0;
  var partial_arg = {
    TAG: "RuleSet",
    _0: rules$1
  };
  var validTickets = Belt_Array.keep(tickets, (function (param) {
          return isValidTicket(partial_arg, param);
        }));
  var ticketCols = Utils$AdventOfCode.transpose(validTickets);
  return Belt_MapString.map(rules$1, (function (__x) {
                return possibleColumns(ticketCols, __x);
              }));
}

function reduceCandidate(candidates) {
  var findNextCandidate = function (candidates) {
    var only1Elem = function (param, v) {
      return v.length === 1;
    };
    return Belt_MapString.findFirstBy(candidates._0, only1Elem);
  };
  var _c = {
    TAG: "ColCandidateSet",
    _0: candidates._0
  };
  var _solved = [];
  while(true) {
    var c = _c;
    var solved = _solved;
    var c$1 = c._0;
    var match = findNextCandidate({
          TAG: "ColCandidateSet",
          _0: c$1
        });
    if (match === undefined) {
      return solved;
    }
    var k = match[0];
    var v$p = Belt_Array.getExn(match[1], 0);
    var solved$p = Belt_Array.concat(solved, [[
            k,
            v$p
          ]]);
    var removeFound = (function(v$p){
    return function removeFound(__x) {
      return Belt_Array.keep(__x, (function (x) {
                    return x !== v$p;
                  }));
    }
    }(v$p));
    _solved = solved$p;
    _c = {
      TAG: "ColCandidateSet",
      _0: Belt_MapString.map(Belt_MapString.remove(c$1, k), removeFound)
    };
    continue ;
  };
}

function parse(data) {
  var trim = function (prim) {
    return prim.trim();
  };
  var intFromStrEx = function (param) {
    return Utils$AdventOfCode.compose(Belt_Int.fromString, Belt_Option.getExn, param);
  };
  var match = Belt_Array.map(Utils$AdventOfCode.splitDoubleNewline(data), (function (x) {
          return Belt_Array.map(Utils$AdventOfCode.splitNewline(x), trim);
        }));
  if (match.length !== 3) {
    throw {
          RE_EXN_ID: "Match_failure",
          _1: [
            "AOC2020_Day16.res",
            103,
            6
          ],
          Error: new Error()
        };
  }
  var rules = match[0];
  var my = match[1];
  var nearby = match[2];
  var parseRange = function (s) {
    var match = Belt_Array.map(s.split("-"), intFromStrEx);
    if (match.length !== 2) {
      throw {
            RE_EXN_ID: "Match_failure",
            _1: [
              "AOC2020_Day16.res",
              106,
              8
            ],
            Error: new Error()
          };
    }
    var a = match[0];
    var b = match[1];
    return {
            TAG: "Range",
            _0: a,
            _1: b
          };
  };
  var parseRule = function (s) {
    var match = s.split(": ");
    if (match.length !== 2) {
      throw {
            RE_EXN_ID: "Match_failure",
            _1: [
              "AOC2020_Day16.res",
              111,
              8
            ],
            Error: new Error()
          };
    }
    var ruleStr = match[0];
    var rangesStr = match[1];
    var match$1 = Belt_Array.map(rangesStr.split(" or "), parseRange);
    if (match$1.length !== 2) {
      throw {
            RE_EXN_ID: "Match_failure",
            _1: [
              "AOC2020_Day16.res",
              112,
              8
            ],
            Error: new Error()
          };
    }
    var r1 = match$1[0];
    var r2 = match$1[1];
    return [
            ruleStr,
            {
              TAG: "Body",
              _0: r1,
              _1: r2
            }
          ];
  };
  var ruleSet = {
    TAG: "RuleSet",
    _0: Belt_MapString.fromArray(Belt_Array.map(rules, parseRule))
  };
  var parseTicket = function (s) {
    return Belt_Array.map(s.split(","), intFromStrEx);
  };
  var myTicket = parseTicket(Belt_Array.getExn(my, 1));
  var nearbyTickets = Belt_Array.map(Belt_Array.sliceToEnd(nearby, 1), parseTicket);
  return [
          ruleSet,
          myTicket,
          nearbyTickets
        ];
}

function solvePart1(data) {
  var match = parse(data);
  return ticketErrorRate(match[0], match[2]);
}

function solvePart2(data) {
  var match = parse(data);
  var myTicket = match[1];
  var pc = possibleColumnsAll(match[0], match[2]);
  var colMapping = reduceCandidate({
        TAG: "ColCandidateSet",
        _0: pc
      });
  return Belt_Array.reduce(Belt_Array.keepMap(colMapping, (function (param) {
                    if (param[0].startsWith("departure")) {
                      return Belt_Array.getExn(myTicket, param[1]);
                    }
                    
                  })), 1, (function (acc, x) {
                return x * acc;
              }));
}

var part1 = ticketErrorRate;

export {
  log ,
  log2 ,
  rangeToString ,
  bodyToString ,
  inRange ,
  matchesRule ,
  validForAnyField ,
  ticketErrorRate ,
  part1 ,
  isValidTicket ,
  possibleColumns ,
  possibleColumnsAll ,
  reduceCandidate ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* Utils-AdventOfCode Not a pure module */
