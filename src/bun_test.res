type result = {exitCode: int}
@module("bun") @taggedTemplate
external sh: (array<string>, array<string>) => promise<result> = "$"

let filename = "index.res"
let flag = "-l"
let result = await sh`ls ${filename} ${flag}`

open RescriptCore.Array
module Array = RescriptCore.Array
let animals = ["🐶", "🐱", "🐷"]
let moreAnimals = [...animals, "🐔", "🐴", "🐮"]
