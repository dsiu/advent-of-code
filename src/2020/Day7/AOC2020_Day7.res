open Belt
let log = Js.Console.log
//open Utils

//let parseLine = l =>

let parse = data =>
  data
  ->Js.String2.split("\n")
  ->Array.map(x => {
    x->Js.String2.trim->Js.String2.replaceByRe(%re("/(\\r\\n|\\r|\\n|\\s)+/g"), " ")
  })

let solvePart1 = data => {
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
