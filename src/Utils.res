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
let dump_mapInt_of_int64 = dump_mapInt_of(_, Int64.to_string)

// mutable map int
let dump_mutableMapInt_of = (f, m) =>
  MutableMap.Int.forEach(m, (k, v) => {
    log(`key:${k->Int.toString}, val:${v->f}`)
  })
let dump_mutableMapInt_of_int = dump_mutableMapInt_of(Int.toString)
let dump_mutableMapInt_of_int64 = dump_mutableMapInt_of(Int64.to_string)

let dump_mutableMapInt_of_int_base2 = dump_mutableMapInt_of(x =>
  x->Js.Int.toStringWithRadix(~radix=2)
)

// mutable map string
let dump_mutableMapString_of = (f, m) =>
  MutableMap.String.forEach(m, (k, v) => {
    log(`key:${k}, val:${v->f}`)
  })
let dump_mutableMapString_of_int = dump_mutableMapString_of(Int.toString)
let dump_mutableMapString_of_int64 = dump_mutableMapString_of(Int64.to_string)

//
// Int / Int64
//
//@scope("Math") @val
@val
external parseInt: (~x: string, ~base: int) => int = "parseInt"
let base2 = Js.Int.toStringWithRadix(_, ~radix=2)

let intFromStringExn = FP_Utils.compose(Int.fromString, Belt.Option.getExn)

let add = (x, y) => x + y
let sub = (x, y) => x - y
let mul = (x, y) => x * y
let div = (x, y) => x / y

// Unsigned Int conversion
let int32ToUint32 = x => {
  open Js.TypedArray2
  Uint32Array.make([x])->Uint32Array.unsafe_get(0)
}

let increaseByInt64 = (v, n) => {
  v->Option.mapWithDefault(n, x => x->Int64.add(n))
}

let increaseBy1L = increaseByInt64(_, 1L)

let increaseBy = (v, n) => {
  v->Option.mapWithDefault(n, x => x + n)
}

let increaseBy1 = increaseBy(_, 1)

//
// Int64
//
let int64FromBitString = str => ("0b" ++ str)->Int64.of_string

//
// list
//
let dump_list = List.forEach(_, log)

//
// strings
//
let splitChars = Js.String2.split(_, "")
let splitNewline = Js.String2.split(_, "\n")
let splitDoubleNewline = Js.String2.split(_, "\n\n")

//
// array
//
let sumIntArray = Array.reduce(_, 0, add)
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

let flatten = (xs: array<array<'a>>) => {
  xs->Array.reduce([], (a, x) => Array.concat(a, x))
}

let maxKeyIntValuePair = Array.reduce(_, ("", 0), (acc, (k, v)) => {
  let (_, va) = acc
  v > va ? (k, v) : acc
})

let minKeyIntValuePair = Array.reduce(_, ("", max_int), (acc, (k, v)) => {
  let (_, va) = acc
  v < va ? (k, v) : acc
})

let maxKeyInt64ValuePair = Array.reduce(_, ("", 0L), (acc, (k, v)) => {
  let (_, va) = acc
  Int64.compare(v, va) > 0 ? (k, v) : acc
})

let minKeyInt64ValuePair = Array.reduce(_, ("", Int64.max_int), (acc, (k, v)) => {
  let (_, va) = acc
  Int64.compare(v, va) < 0 ? (k, v) : acc
})

//
// Map / HashMap
//

let hashMapStringUpdate = (h, k, f) => {
  h->HashMap.String.set(
    k,
    h->HashMap.String.get(k)->Option.mapWithDefaultU(f(None), (. x) => f(Some(x))),
  )
  h
}
