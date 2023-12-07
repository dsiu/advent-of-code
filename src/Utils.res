open RescriptCore
module Map = Belt.Map
module MutableMap = Belt.MutableMap
module HashMap = Belt.HashMap

let identity = Stdlib.Function.identity

// Logging
let log = Console.log
@val @scope("console") external consoleDir: 'a => unit = "dir"

/**
 Printable
 */
module Printable = {
  /**
    MapString
    @param: m
  */
  module MapString = {
    let toString = (m, f) =>
      Map.String.reduce(m, "", (a, k, v) => {
        a ++ `key:${k}, val:${v->f}\n`
      })

    module String = {
      let toString = m => toString(m, identity)
    }

    module Int = {
      let toString = m => toString(m, Int.toString)
    }
  }

  /**
    MapInt
  */
  module MapInt = {
    let toString = (m, f) => {
      Map.Int.reduce(m, "", (a, k, v) => {
        a ++ `key:${k->Int.toString}, val:${f(v)}\n`
      })
    }
    module String = {
      let toString = m => toString(m, identity)
    }

    module Int = {
      /**
       @returns String
       @param {Map} m the `map`
       @param b the other
      */
      let toString = m => toString(m, Int.toString)
    }

    module Int64 = {
      let toString = m => toString(m, Int64.to_string)
    }
  }

  // mutable map int
  module MutableMapInt = {
    let toString = (m, f) =>
      MutableMap.Int.reduce(m, "", (a, k, v) => {
        a ++ `key:${k->Int.toString}, val:${v->f}\n`
      })

    module Int = {
      let toString = m => toString(m, Int.toString)
    }

    module Int64 = {
      let toString = m => toString(m, Int64.to_string)
    }

    module IntBase2 = {
      let toString = m => toString(m, x => x->RescriptCore.Int.toStringWithRadix(~radix=2))
    }
  }

  // mutable map string
  module MutableMapString = {
    let toString = (m, f) =>
      MutableMap.String.reduce(m, "", (a, k, v) => {
        a ++ `key:${k}, val:${v->f}\n`
      })
    module Int = {
      let toString = m => toString(m, Int.toString)
    }
    module Int64 = {
      let toString = m => toString(m, Int64.to_string)
    }
  }

  module Array = {
    let toString = (a, f) => {
      "[" ++ a->Array.map(f)->Array.joinWith(",") ++ "]"
    }
  }

  module List = {
    let toString = (a, f) => {
      a->List.reduce("{", (a, v) => a ++ f(v) ++ ",") ++ "}"
    }
  }
}

//
// Int / Int64
//
//@scope("Math") @val
@val
external parseInt: (~x: string, ~base: int) => int = "parseInt"
let base2 = Int.toStringWithRadix(_, ~radix=2)

let compose = Stdlib.Function.compose
let intFromStringExn = compose(String.trim, compose(Int.fromString(~radix=10), Option.getUnsafe))

let add = (x, y) => x + y
let sub = (x, y) => x - y
let mul = (x, y) => x * y
let div = (x, y) => x / y

// Unsigned Int conversion
let int32ToUint32 = x => {
  Js.TypedArray2.Uint32Array.make([x])->Js.TypedArray2.Uint32Array.unsafe_get(0)
}

let increaseByInt64 = (v, n) => {
  v->Option.mapOr(n, x => x->Int64.add(n))
}

let increaseBy1L = increaseByInt64(_, 1L)

let increaseBy = (v, n) => {
  v->Option.mapOr(n, x => x + n)
}

let increaseBy1 = increaseBy(_, 1)

//
// Int64
//
let int64FromBitString = str => ("0b" ++ str)->Int64.of_string

//
// strings
//
let splitChars = String.split(_, "")
let splitNewline = String.split(_, "\n")
let splitDoubleNewline = String.split(_, "\n\n")

//
// array
//
let sumIntArray = Array.reduce(_, 0, add)
let mulIntArray = Array.reduce(_, 1, mul)
let join = Array.joinWith(_, "")

// sum up elements of array from ~offset with ~len (same as Array.slice)
let sumRange = (xs, ~offset, ~len) => {
  let elems = xs->Belt.Array.slice(~offset, ~len)
  let total = ref(0)
  elems->Array.forEach(x => total := total.contents + x)
  total.contents
}

let maxIntInArray = xs => {
  let sorted = xs->Array.toSorted(Int.compare)
  sorted->Array.getUnsafe(sorted->Array.length - 1)
}

let minIntInArray = xs => {
  let sorted = xs->Array.toSorted(Int.compare)
  sorted->Array.getUnsafe(0)
}

let flatten = (xs: array<array<'a>>) => {
  xs->Array.reduce([], (a, x) => Array.concat(a, x))
}

// ref: https://blog.shaynefletcher.org/2017/08/transpose.html
// ref: https://github.com/nyinyithann/rescript-js-array2-extension/blob/main/src/JsArray2Ex.res

let transpose = JsArray2Ex.transpose

let maxKeyIntValuePair = Array.reduce(_, ("", min_int), (acc, (k, v)) => {
  let (_, va) = acc
  v > va ? (k, v) : acc
})

let minKeyIntValuePair = Array.reduce(_, ("", max_int), (acc, (k, v)) => {
  let (_, va) = acc
  v < va ? (k, v) : acc
})

let maxKeyInt64ValuePair = Array.reduce(_, ("", Int64.min_int), (acc, (k, v)) => {
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
  h->HashMap.String.set(k, h->HashMap.String.get(k)->Option.mapOr(f(None), x => f(Some(x))))
  h
}

let mutableMapStringUpdate = (h, k, f) => {
  h->MutableMap.String.set(k, h->MutableMap.String.get(k)->Option.mapOr(f(None), x => f(Some(x))))
  h
}
