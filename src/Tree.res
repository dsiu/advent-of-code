open Belt
exception Not_Expected(string)

// Type
//
type rec tree<'a> =
  | Leaf('a)
  | Pair(tree<'a>, tree<'a>)

// show tree
//
let rec treeToString = e => {
  switch e {
  | Leaf(i) => i->Int.toString
  | Pair((e1, e2)) => {
      let s1 = e1->treeToString
      let s2 = e2->treeToString
      j`[$s1, $s2]`
    }
  }
}

// zipper
// https://wiki.haskell.org/Zipper
//
type rec cxt<'a> =
  | Top
  | L(cxt<'a>, tree<'a>)
  | R(tree<'a>, cxt<'a>)

let rec cxtToString = c => {
  switch c {
  | Top => "Top"
  | L(c, t) => {
      let t_str = t->treeToString
      let c_str = c->cxtToString
      j`L($c_str, $t_str)`
    }
  | R(t, c) => {
      let t_str = t->treeToString
      let c_str = c->cxtToString
      j`R($t_str, $c_str)`
    }
  }
}

type loc<'a> = Loc(tree<'a>, cxt<'a>)

let locToString = (Loc(t, c): loc<'a>) => {
  let t_str = t->treeToString
  let c_str = c->cxtToString
  j`Loc[ cxt = $c_str, tree = $t_str ]`
}

let left = (Loc(t, c)) => {
  switch t {
  | Pair(l, r) => Loc(l, L(c, r))
  | _ => raise(Not_Expected("left: not a Pair"))
  }
}

let right = (Loc(t, c)) => {
  switch t {
  | Pair(l, r) => Loc(r, R(l, c))
  | _ => raise(Not_Expected("right: not a Pair"))
  }
}

let top = t => Loc(t, Top)

let up = (Loc(t, c)) => {
  switch c {
  | L(c, r) => Loc(Pair(t, r), c)
  | R(l, c) => Loc(Pair(l, t), c)
  | Top => raise(Not_Expected("up: not a L or R"))
  }
}

let rec upmost = (Loc(_, c) as l) => {
  switch c {
  | Top => l
  | _ => upmost(up(l))
  }
}

type modify_<'a> = (loc<'a>, tree<'a> => tree<'a>) => loc<'a>
let modify: modify_<'a> = (Loc(t, c), f) => Loc(t->f, c)
