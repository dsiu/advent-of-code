include Belt.Array

// Basic Functions
let append = concat
let head = getExn(_, 0)
let last = a => getExn(a, a->length - 1)
let tail = sliceToEnd(_, 1)
// let init =
// let uncons =
let singleton = make(1, _)

// Array transformations

/**
  flatMap (ie: bind) on Array
 */
let flatMap: (array<'a>, 'a => array<'b>) => array<'b> = (xs, f) => {
  reduce(map(xs, f), [], concat)
}

let arrayToOption = get(_, 0)

/**
 fold left on Array
 */
let foldLeft: (array<'a>, ('a, 'a) => 'a) => 'a = (xs, f) => {
  let init = xs->getExn(0)
  let rest = xs->sliceToEnd(1)
  rest->reduce(init, f)
}

/**
  fold right on Array
 */
let foldRight: (array<'a>, ('a, 'a) => 'a) => 'a = (xs, f) => {
  let end = xs->length - 1
  let init = xs->getExn(end)
  let rest = xs->slice(~offset=0, ~len=end)
  rest->reduceReverse(init, f)
}

/**
  apply f(x,y) for each x in a and each y in b ONLY if f(x,y) returns Some()
  returns result in array
*/
let combinationIf2: (array<'a>, array<'b>, (. 'a, 'b) => option<'r>) => array<'r> = (a, b, f) => {
  let ret = ref([])
  a->forEach(x => {
    b->forEach(y => {
      switch f(. x, y) {
      | Some(r) => ret := ret.contents->concat([r])
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
let combination2: (array<'a>, array<'b>, (. 'a, 'b) => 'r) => array<'r> = (a, b, f) => {
  combinationIf2(a, b, (. x, y) => Some(f(. x, y)))
}

/**
  apply f(x,y,z) for each x in a, each y in b, and each z in c ONLY if f(x,y,z) returns Some()
  returns result in array
*/
let combinationIf3: (array<'a>, array<'b>, array<'c>, (. 'a, 'b, 'c) => option<'r>) => array<'r> = (
  a,
  b,
  c,
  f,
) => {
  let ret = ref([])
  a->forEach(x => {
    b->forEach(y => {
      c->forEach(
        z => {
          switch f(. x, y, z) {
          | Some(r) => ret := ret.contents->concat([r])
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
  combinationIf3(a, b, c, (. x, y, z) => Some(f(. x, y, z)))
}

/**
  apply f(x,y,z,w) for each x in a, each y in b, each z in c, and each w in d, ONLY if f(x,y,z,w) returns Some()
  returns result in array
*/
let combinationIf4: (
  array<'a>,
  array<'b>,
  array<'c>,
  array<'d>,
  (. 'a, 'b, 'c, 'd) => option<'e>,
) => array<'e> = (a, b, c, d, f) => {
  let ret = ref([])
  a->forEach(x => {
    b->forEach(y => {
      c->forEach(
        z => {
          d->forEach(
            w => {
              switch f(. x, y, z, w) {
              | Some(r) => ret := ret.contents->concat([r])
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
let combination4: (
  array<'a>,
  array<'b>,
  array<'c>,
  array<'d>,
  (. 'a, 'b, 'c, 'd) => 'r,
) => array<'r> = (a, b, c, d, f) => {
  combinationIf4(a, b, c, d, (. x, y, z, w) => Some(f(. x, y, z, w)))
}