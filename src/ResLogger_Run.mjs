// Generated by ReScript, PLEASE EDIT WITH CARE

import * as ResLogger$AdventOfCode from "./ResLogger.mjs";

var Log = ResLogger$AdventOfCode.make("ResLogger_Run-AdventOfCode");

var LogR = ResLogger$AdventOfCode.make("Logger Run");

Log.info("Starting");

var error = {
  a: 1,
  b: 2
};

Log.error2("Startup error", error);

LogR.info("end");

export {
  Log ,
  LogR ,
  error ,
}
/* Log Not a pure module */
