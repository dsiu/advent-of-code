include Belt.List

/**
  flatMap (ie: bind) on List
*/
let flatMap = (xs, f) => {
  reduce(map(xs, f), list{}, concat)
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
let combinationIf2: (list<'a>, list<'b>, (. 'a, 'b) => option<'c>) => list<'c> = (a, b, f) => {
  a->reduceU(list{}, (. acc, x) => {
    acc->concat(
      b->reduceU(list{}, (. acc, y) => {
        switch f(. x, y) {
        | Some(r) => acc->concat(list{r})
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
let combination2: (list<'a>, list<'b>, (. 'a, 'b) => 'c) => list<'c> = (a, b, f) => {
  combinationIf2(a, b, (. x, y) => Some(f(. x, y)))
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
