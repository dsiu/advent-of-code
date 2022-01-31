open Belt
open Utils
let log = Js.Console.log

// corrupted = closes with wrong character
// eg: (], {()()()>, (((()))}

exception ParseError(string)

module Token = {
  type open_bracket = [#"(" | #"[" | #"{" | #"<"]
  type close_bracket = [#")" | #"]" | #"}" | #">"]

  type t = [open_bracket | close_bracket]

  let isOpenBracket = token => {
    switch token {
    | #...open_bracket => true
    | _ => false
    }
  }

  let isCloseBracket = token => {
    switch token {
    | #...close_bracket => true
    | _ => false
    }
  }

  let make = (c): t => {
    switch c {
    | "(" => #"("
    | "[" => #"["
    | "{" => #"{"
    | "<" => #"<"
    | ")" => #")"
    | "]" => #"]"
    | "}" => #"}"
    | ">" => #">"
    | e => raise(ParseError(`${e} is not supported`))
    }
  }
}

module ParseTree = {
  type token_at = TokenAt(Token.t, int)

  type rec t =
    | Empty
    | Node({l: token_at, tl: list<t>, r: token_at})

  let rec map = (t, f) => {
    switch t {
    | Empty => Empty
    | Node({l: b1, tl: l, r: b2}) => Node({l: b1->f, tl: l->List.map(map(_, f)), r: b2->f})
    }
  }

  let makeNode = (l, lp, r, rp) => {
    Node({l: TokenAt(l->Token.make, lp), tl: list{Empty}, r: TokenAt(r->Token.make, rp)})
  }

  let addChildren = (t, children) => {
    switch t {
    | Node({l, tl: _, r}) => Node({l: l, tl: children, r: r})
    | Empty => Empty
    }
  }
}

let parse = data =>
  data->splitNewline->Array.map(FP_Utils.compose(Js.String2.trim, Utils.splitChars))

let solvePart1 = data => {
  data->ignore
  //  data->parse->Js.log
  let parent = ParseTree.makeNode("(", 1, ")", 2)
  let t =
    parent->ParseTree.addChildren(list{
      ParseTree.makeNode("[", 3, "]", 4),
      ParseTree.makeNode("<", 5, ">", 6),
    })

  t
  ->ParseTree.map(x => {
    x->Js.log
    x
  })
  ->ignore

  t
  ->ParseTree.map((TokenAt(a, i)) => {
    switch a {
    | #...Token.open_bracket => Js.log2(i, "Open")->ignore
    | #...Token.close_bracket => Js.log2(i, "Close")->ignore
    }
    TokenAt(a, i)
  })
  ->ignore

  1
}

let solvePart2 = data => {
  data->ignore
  2
}
