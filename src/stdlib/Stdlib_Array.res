include Belt.Array

/**
  flatMap (ie: bind) on Array
 */
let flatMapArray: (array<'a>, 'a => array<'b>) => array<'b> = (xs, f) => {
  module Array = Belt.Array

  Array.reduce(Array.map(xs, f), [], Array.concat)
}

let arrayToOption = Belt.Array.get(_, 0)

/**
 fold left on Array
 */
let foldLeftArray: (array<'a>, ('a, 'a) => 'a) => 'a = (xs, f) => {
  module Array = Belt.Array

  let init = xs->Array.getExn(0)
  let rest = xs->Array.sliceToEnd(1)
  rest->Array.reduce(init, f)
}

/**
  fold right on Array
 */
let foldRightArray: (array<'a>, ('a, 'a) => 'a) => 'a = (xs, f) => {
  module Array = Belt.Array

  let end = xs->Array.length - 1
  let init = xs->Array.getExn(end)
  let rest = xs->Array.slice(~offset=0, ~len=end)
  rest->Array.reduceReverse(init, f)
}

/**
  apply f(x,y) for each x in a and each y in b ONLY if f(x,y) returns Some()
  returns result in array
*/
let combinationIfArray2: (array<'a>, array<'b>, (. 'a, 'b) => option<'r>) => array<'r> = (
  a,
  b,
  f,
) => {
  module Array = Js.Array2

  let ret = ref([])
  a->Array.forEach(x => {
    b->Array.forEach(y => {
      switch f(. x, y) {
      | Some(r) => ret := ret.contents->Array.concat([r])
      | None => ()
      }
    })
  })

  ret.contents
}

/**
  apply f(x,y) for each x in a and and each y in b
  returns result in array
*/
let combinationArray2: (array<'a>, array<'b>, (. 'a, 'b) => 'r) => array<'r> = (a, b, f) => {
  module Array = Js.Array2
  combinationIfArray2(a, b, (. x, y) => Some(f(. x, y)))
}

/**
  apply f(x,y,z) for each x in a, each y in b, and each z in c ONLY if f(x,y,z) returns Some()
  returns result in array
*/
let combinationIfArray3: (
  array<'a>,
  array<'b>,
  array<'c>,
  (. 'a, 'b, 'c) => option<'r>,
) => array<'r> = (a, b, c, f) => {
  module Array = Js.Array2

  let ret = ref([])
  a->Array.forEach(x => {
    b->Array.forEach(y => {
      c->Array.forEach(
        z => {
          switch f(. x, y, z) {
          | Some(r) => ret := ret.contents->Array.concat([r])
          | None => ()
          }
        },
      )
    })
  })

  ret.contents
}

/**
  apply f(x,y,z) for each x in a, y in b, z in c
  returns result in array
*/
let combinationArray3: (array<'a>, array<'b>, array<'c>, (. 'a, 'b, 'c) => 'r) => array<'r> = (
  a,
  b,
  c,
  f,
) => {
  combinationIfArray3(a, b, c, (. x, y, z) => Some(f(. x, y, z)))
}

/**
  apply f(x,y,z,w) for each x in a, each y in b, each z in c, and each w in d, ONLY if f(x,y,z,w) returns Some()
  returns result in array
*/
let combinationIfArray4: (
  array<'a>,
  array<'b>,
  array<'c>,
  array<'d>,
  (. 'a, 'b, 'c, 'd) => option<'e>,
) => array<'e> = (a, b, c, d, f) => {
  module Array = Js.Array2

  let ret = ref([])
  a->Array.forEach(x => {
    b->Array.forEach(y => {
      c->Array.forEach(
        z => {
          d->Array.forEach(
            w => {
              switch f(. x, y, z, w) {
              | Some(r) => ret := ret.contents->Array.concat([r])
              | None => ()
              }
            },
          )
        },
      )
    })
  })

  ret.contents
}

/**
  apply f(x,y,z) for each x in a, y in b, z in c, w in d
  returns result in array
*/
let combinationArray4: (
  array<'a>,
  array<'b>,
  array<'c>,
  array<'d>,
  (. 'a, 'b, 'c, 'd) => 'r,
) => array<'r> = (a, b, c, d, f) => {
  combinationIfArray4(a, b, c, d, (. x, y, z, w) => Some(f(. x, y, z, w)))
}
