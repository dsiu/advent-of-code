open Belt
let log = Js.Console.log
open Utils

module Passport = {
  type t = Map.String.t<string>

  let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  let optionalFields = ["cid"]

  let kvStrToMap = xs => {
    xs->Array.reduce(Map.String.empty, (a, s) => {
      let kvs = s->Js.String2.split(":")
      a->Map.String.set(kvs[0]->Option.getExn, kvs[1]->Option.getExn)
    })
  }

  let containsAllFields = (t, fields) => {
    fields->Array.reduce(true, (a, f) => {
      a && t->Map.String.has(f)
    })
  }

  let containsAllOptionalFields = containsAllFields(_, optionalFields)
  let containsAllRequiredFields = containsAllFields(_, requiredFields)

  let isValid = containsAllRequiredFields

  let make = s => {
    let kvStrs = s->Js.String2.split(" ")
    kvStrs->kvStrToMap
  }
}

let parse = data =>
  data
  ->Js.String2.split("\n\n")
  ->Array.map(x => {
    x->Js.String2.trim->Js.String2.replaceByRe(%re("/(\\r\\n|\\r|\\n|\\s)+/g"), " ")
  })

let solvePart1 = data => {
  let parsed = data->parse
  parsed->log
  parsed->Array.length->log
  let p = parsed->Array.map(Passport.make)
  p->Array.length->log
  p->Array.forEach(dump_mapString_of_string)
  p->Array.map(Passport.isValid)->log
}

//let solvePart2 = data => {
//  let parsed = data->Js.String2.split("\n\n")->Array.map(Js.String2.trim)
//  2
//}
