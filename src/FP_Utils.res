//
// List
//

/**
  flatMap (ie: bind) on List
*/
let flatMapList = (xs, f) => {
  open Belt
  List.reduce(List.map(xs, f), list{}, List.concat)
}

let listToOption = l => {
  switch l {
  | list{} => None
  | list{h, ..._} => Some(h)
  }
}

/**
  apply f(x,y) for each x in a and each y in b ONLY if f(x,y) returns Some()
  returns result in list
*/
let combinationIfList2: (list<'a>, list<'b>, (. 'a, 'b) => option<'c>) => list<'c> = (a, b, f) => {
  module List = Belt.List

  a->List.reduceU(list{}, (. acc, x) => {
    acc->List.concat(
      b->List.reduceU(list{}, (. acc, y) => {
        switch f(. x, y) {
        | Some(r) => acc->List.concat(list{r})
        | None => acc
        }
      }),
    )
  })
}

/**
  apply f(x,y) for each x in a and and each y in b
  returns result in list
*/
let combinationList2: (list<'a>, list<'b>, (. 'a, 'b) => 'c) => list<'c> = (a, b, f) => {
  combinationIfList2(a, b, (. x, y) => Some(f(. x, y)))
}

//
// Array
//

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

//
// Options
//
/**
  option(a,b): returns a if a is Some(_) other wise return b
 */
let optionOr: (option<'a>, option<'a>) => option<'a> = (a, b) => {
  switch a {
  | Some(_) => a
  | None => b
  }
}

// Iterate / Unfold2
// ref: https://thealmarty.com/2018/12/18/using-unfolds-to-iterate-in-haskell-and-ocaml/

// The unfold function takes in a predicate (p) and a function (g) which takes an input (b).
let rec unfold = (p, g, b) =>
  if p(b) {
    list{}
  } else {
    switch g(b) {
    | (a, b') => list{a, ...unfold(p, g, b')}
    }
  }

// The iterate function takes in a function (f), and uses the unfold function with p and g passed. b
// is to be passed.

// This function is equivalent to the Haskell iterate function.
// nOops!  We get a stack overflow error!  Because OCaml is eager by default, it tells us that there
// is a never ending loop as it runs out of stack.  If we give unfold a predicate, the list can end:

// let iterate = f => unfold(x => false, x => (x, f(x)))

/* Giving unfold a predicate of x > 20 returns a list. */
// let iterate_p = f => unfold(x => x > 20, x => (x, f(x)))

// Common FP utils
//
let identity: 'a => 'a = (a: 'a) => a
let eq: ('a, 'a) => bool = (x, y) => x === y

/**
  composeU(f, g, x) = g(f(x))
 */
let composeU: ((. 'a) => 'b, (. 'b) => 'c, 'a) => 'c = (f, g, x) => g(. f(. x))

/**
  compose(f,g,x) = g(f(x))
 */
let compose: ('a => 'b, 'b => 'c, 'a) => 'c = (f, g, x) => g(f(x))

let compose3 = (f, g, h, x) => h(g(f(x)))
let compose4 = (f, g, h, i, x) => i(h(g(f(x))))

let composeN = fs => {
  //  fs->Array.sliceToEnd(1)->Array.reduce(fs->Array.getExn(0), (a, f) => compose(a, f))
  fs->foldLeftArray(compose)
}
