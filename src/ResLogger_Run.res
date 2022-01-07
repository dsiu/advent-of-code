module Log = unpack(ResLogger.make(__MODULE__))
module LogR = unpack(ResLogger.make("Logger Run"))

Log.info("Starting")

// ...

let error = {"a": 1, "b": 2}
Log.error2("Startup error", error)

LogR.info("end")
