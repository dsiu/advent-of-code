open Belt
open Utils
let log = Js.Console.log

module Stack = Stack_Array
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

  let matches = (~left: t, ~right: t) => {
    switch (left, right) {
    | (#"(", #")")
    | (#"[", #"]")
    | (#"{", #"}")
    | (#"<", #">")
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

  let tokenize = (xs): array<token_at> => {
    xs->Array.mapWithIndex((i, x) => TokenAt(x->Token.make, i))
  }

  type rec t =
    | Empty
    | Node({l: token_at, tl: list<t>, r: token_at})

  let rec map = (t, f) => {
    switch t {
    | Empty => Empty
    | Node({l: b1, tl: l, r: b2}) => Node({l: b1->f, tl: l->List.map(map(_, f)), r: b2->f})
    }
  }

  let makeNode = (l, r) => {
    Node({l: l, tl: list{Empty}, r: r})
  }

  let makeNodeFromStr = (l, li, r, ri) => {
    makeNode(TokenAt(l->Token.make, li), TokenAt(r->Token.make, ri))
  }

  let addChildren = (t, children) => {
    switch t {
    | Node({l, tl: _, r}) => Node({l: l, tl: children, r: r})
    | Empty => Empty
    }
  }

  let makeParseTree = xs => {
    let rec inner = (inputs, idx, tree, stack) => {
      switch inputs {
      | list{} => tree
      | list{this, ...rest} => {
          let last = stack->Stack.peek->Option.getExn
          let TokenAt(last_token, _) = last
          let TokenAt(this_token, _) = this

          Token.matches(~left=last_token, ~right=this_token)
            ? {
                inner(rest, idx + 1, makeNode(last, this), stack)
              }
            : {
                inner(rest, idx + 1, tree, stack->Stack.push(this))
              }
        }
      }
    }
    inner(xs->tokenize->List.fromArray, 0, Empty, [])
  }
}

let parse = data =>
  data->splitNewline->Array.map(FP_Utils.compose(Js.String2.trim, Utils.splitChars))

let solvePart1 = data => {
  data->ignore
  //  data->parse->Js.log
  let parent = ParseTree.makeNodeFromStr("(", 1, ")", 2)
  let t =
    parent->ParseTree.addChildren(list{
      ParseTree.makeNodeFromStr("[", 3, "]", 4),
      ParseTree.makeNodeFromStr("<", 5, ">", 6),
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
