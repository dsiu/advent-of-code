open Belt
let log = Js.Console.log

// map string
let dump_mapString_of = (f, m) =>
  Map.String.forEach(m, (k, v) => {
    log(`key:${k}, val:${v->f}`)
  })

let dump_mapString_of_int = dump_mapString_of(Int.toString)
let dump_mapString_of_string = dump_mapString_of(a => a)

// map int
let dump_mapInt_of = (m, f) =>
  Map.Int.forEach(m, (k, v) => {
    log(`key:${k->Int.toString}, val:${f(v)}`)
  })
let dump_mapInt_of_int = dump_mapInt_of(_, Int.toString)

// mutable map int
let dump_mutableMapInt_of = (f, m) =>
  MutableMap.Int.forEach(m, (k, v) => {
    log(`key:${k->Int.toString}, val:${v->f}`)
  })
let dump_mutableMapInt_of_int = dump_mutableMapInt_of(Int.toString)

// list
let dump_list = List.forEach(_, log)
