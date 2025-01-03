let log = Console.log

let data = `Time:      7  15   30
            Distance:  9  40  200`

module S = RescriptSchema.S
let timeSchema = S.literal("Time:")

data->S.parseOrThrow(timeSchema)->log
//let l = S.list(S.int)

//"7 15 30"->S.parseOrThrow(l)->log
