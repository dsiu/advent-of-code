// Generated by ReScript, PLEASE EDIT WITH CARE

import * as JS_Debug from "./JS_Debug.res.mjs";

let Log = JS_Debug.make("JS_Debug_Run");

let LogR = JS_Debug.make("Logger Run");

Log.debug("Starting");

LogR.debug("Starting");

let error = {
  a: 1,
  b: 2
};

Log.debug("end");

LogR.debug("end");

let b = {
  propb: "valueb"
};

export {
  Log,
  LogR,
  error,
  b,
}
/* Log Not a pure module */