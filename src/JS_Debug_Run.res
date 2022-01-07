module Log = unpack(JS_Debug.make(__MODULE__))
module LogR = unpack(JS_Debug.make("Logger Run"))

Log.debug("Starting")
LogR.debug("Starting")

// ...

let error = {"a": 1, "b": 2}
//Log.error2("Startup error", error)

Log.debug("end")
LogR.debug("end")

//let jsd2 = JS_Debug.JSD.createDebug2("debug2")

type b = {propb: string}
let b = {"propb": "valueb"}
//JS_Debug.JSD.debug2(. jsd2, "this is b", b)

//let c = 1.1111
//jsd2->JS_Debug.JSD.debug2("c", c)
