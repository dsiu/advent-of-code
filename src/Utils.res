open Belt

// Logging
let log = Js.Console.log
@val @scope("console") external consoleDir: 'a => unit = "dir"

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

// strings
let splitChars = Js.String2.split(_, "")
let splitNewline = Js.String2.split(_, "\n")
let splitDoubleNewline = Js.String2.split(_, "\n\n")

// array
let sum = (a, x) => a + x
let sumIntArray = Array.reduce(_, 0, sum)
let join = Js.Array2.joinWith(_, "")

// sum up elements of array from ~offset with ~len (same as Array.slice)
let sumRange = (xs, ~offset, ~len) => {
  let elems = xs->Array.slice(~offset, ~len)
  let total = ref(0)
  elems->Array.forEach(x => total := total.contents + x)
  total.contents
}

let maxIntInArray = xs => {
  let sorted = xs->SortArray.Int.stableSort
  sorted->Array.getExn(sorted->Array.length - 1)
}

let minIntInArray = xs => {
  let sorted = xs->SortArray.Int.stableSort
  sorted->Array.getExn(0)
}
