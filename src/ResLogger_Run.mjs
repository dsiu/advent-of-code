// Generated by ReScript, PLEASE EDIT WITH CARE

import * as ResLogger$AdventOfCode from "./ResLogger.mjs";

let Log = ResLogger$AdventOfCode.make("ResLogger_Run-AdventOfCode");

let LogR = ResLogger$AdventOfCode.make("Logger Run");

Log.info("Starting");

let error = {
  a: 1,
  b: 2
};

Log.error2("Startup error", error);

LogR.info("end");

export {
  Log,
  LogR,
  error,
}
/* Log Not a pure module */
