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

  let isValidPart1 = containsAllRequiredFields

  let getFieldSafeAndValidate = (t, field, f) => {
    let v = t->Map.String.get(field)
    switch v {
    | Some(s) => s->f
    | None => false
    }
  }

  let validateStringAsInt = (s, f) => {
    let n = s->Int.fromString
    switch n {
    | Some(i) => i->f
    | None => false
    }
  }

  let between = (x, a, b) => {
    x >= a && x <= b
  }

  let isValidByr = getFieldSafeAndValidate(_, "byr", validateStringAsInt(_, between(_, 1920, 2002)))
  let isValidIyr = getFieldSafeAndValidate(_, "iyr", validateStringAsInt(_, between(_, 2010, 2020)))
  let isValidEyr = getFieldSafeAndValidate(_, "eyr", validateStringAsInt(_, between(_, 2020, 2030)))

  // wrong
  let eclValidator = s => {
    let eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    eyeColors->Array.some(x => x == s)
  }

  let isValidEcl = getFieldSafeAndValidate(_, "ecl", eclValidator)

  let lengthIs = (s, l) => {s->Js.String2.length == l}

  let charIsInt = s =>
    switch Int.fromString(s) {
    | Some(_) => true
    | None => false
    }

  let charIsHex = s => (s >= "0" && s <= "9") || (s >= "a" && s <= "f")

  let validateCharsInString = (s, f) => {
    s
    ->Js.String2.split("")
    ->Array.reduce(true, (a, x) => {
      a && x->f
    })
  }

  let pidValidator = s => {
    s->lengthIs(_, 9) && s->validateCharsInString(charIsInt)
  }

  let isValidPid = getFieldSafeAndValidate(_, "pid", pidValidator)

  let hclValidator = s => {
    s->lengthIs(_, 7) &&
    s->Js.String2.slice(~from=0, ~to_=1) == "#" &&
    s->Js.String2.slice(~from=1, ~to_=7)->validateCharsInString(charIsHex)
  }

  let isValidHcl = getFieldSafeAndValidate(_, "hcl", hclValidator)

  let hgtRe = %re("/(\d+)(in|cm)/i")
  let hgtValidator = s => {
    switch s->Js.Re.exec_(hgtRe, _) {
    | Some(x) => {
        let a = x->Js.Re.captures
        let h = a[1]->Option.getExn->Js.Nullable.toOption->Option.getExn
        let u = a[2]->Option.getExn->Js.Nullable.toOption->Option.getExn
        switch (u, Int.fromString(h)) {
        | ("cm", Some(h)) => h->between(_, 150, 193)
        | ("in", Some(h)) => h->between(_, 56, 76)
        | (_, _) => false
        }
      }
    | None => false
    }
  }

  let isValidHgt = getFieldSafeAndValidate(_, "hgt", hgtValidator)

  let isValidPart2 = t => {
    let validators = [
      containsAllRequiredFields,
      isValidByr,
      isValidIyr,
      isValidEyr,
      isValidHgt,
      isValidHcl,
      isValidEcl,
      isValidPid,
    ]
    validators->Array.reduce(true, (a, f) => a && t->f)
  }

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
  //  parsed->log
  //  parsed->Array.length->log
  let p = parsed->Array.map(Passport.make)
  //  p->Array.length->log
  //  p->Array.forEach(dump_mapString_of_string)
  p->Array.map(Passport.isValidPart1)->Array.keep(a => a)->Array.length
}

let solvePart2 = data => {
  let parsed = data->parse
  //  parsed->log
  //  parsed->Array.length->log
  let p = parsed->Array.map(Passport.make)
  //  p->Array.length->log
  //  p->Array.forEach(dump_mapString_of_string)
  p->Array.map(Passport.isValidPart2)->Array.keep(a => a)->Array.length
}
