open Belt
open Utils
let log = Js.Console.log

module Stack = Stack_Array
// corrupted = closes with wrong character
// eg: (], {()()()>, (((()))}

exception ParseError(string)
exception NotSupported(string)

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
    | (#"<", #">") => true
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

  let toString = (x: t) => {
    (x :> string)
  }
}

module ParseTree = {
  type token_at = TokenAt(Token.t, int)

  let tokenAtToString = (TokenAt(t, i)) => {
    "'" ++ t->Token.toString ++ "'" ++ ":" ++ i->Int.toString
  }

  let tokenize = (xs): array<token_at> => {
    xs->Array.mapWithIndex((i, x) => TokenAt(x->Token.make, i))
  }

  type rec t =
    | Empty
    | Node({l: token_at, tl: t, r: token_at})
    | NodeList(list<t>)

  let rec map = (t, f) => {
    switch t {
    | Node({l: b1, tl: Empty, r: b2}) => Node({l: b1->f, tl: Empty, r: b2->f})
    | Node({l: _, tl: Node(_), r: _}) => raise(NotSupported("nested nodes"))
    | Node({l: b1, tl: NodeList(tl), r: b2}) =>
      Node({
        l: b1->f,
        tl: map(NodeList(tl), f),
        r: b2->f,
      })
    | NodeList(tl) => NodeList(tl->List.map(map(_, f)))
    | Empty => Empty
    }
  }

  let rec toString = t => {
    switch t {
    | Empty => "Empty"
    | Node({l: b1, tl: Empty, r: b2}) =>
      `Node(${b1->tokenAtToString}, tl: Empty, ${b2->tokenAtToString}})`
    | Node({l: b1, tl, r: b2}) =>
      `Node(${b1->tokenAtToString}, tl: ${tl->toString}, ${b2->tokenAtToString}})`
    | NodeList(tl) =>
      "NodeList:{ " ++ tl->List.map(toString)->List.toArray->Js.Array2.joinWith(", ") ++ " }"
    }
  }

  let makeNode = (l, r) => {
    Node({l: l, tl: Empty, r: r})
  }

  let makeNodeFromStr = (l, li, r, ri) => {
    makeNode(TokenAt(l->Token.make, li), TokenAt(r->Token.make, ri))
  }

  let rec add = (t, children) => {
    switch (t, children) {
    | (Node({tl: Empty, _}) as n, Node(_) as c) => NodeList(list{n, c})
    | (Node({l, tl: Empty, r}), NodeList(_) as c) => Node({l: l, tl: c, r: r})

    | (Node({l, tl: NodeList(tl), r}), _ as c) => Node({l: l, tl: add(NodeList(tl), c), r: r})

    | (Node({tl: Node(_)}), _) => raise(NotSupported("nested nodes"))

    | (NodeList(tl), Node(c)) => NodeList(List.concat(tl, list{Node(c)}))
    | (NodeList(tl), NodeList(cl)) => NodeList(List.concat(tl, cl))

    | (Empty, c) => c
    | (t, Empty) => t
    }
  }

  let makeParseTree = xs => {
    let rec inner = (inputs, tree, stack) => {
      switch inputs {
      | list{} => (tree, stack)
      | list{this, ...rest} => {
          Js.log2("processing", this)
          Js.log2("tree", tree)
          let last = stack->Stack.peek
          //          let TokenAt(last_token, _) = last
          let TokenAt(this_token, _) = this

          switch (this_token->Token.isCloseBracket, last) {
          | (true, Some(last)) => {
              let (_, new_stack) = stack->Stack.pop
              // check matching?
              // check if there is last token in stack
              inner(rest, tree->add(makeNode(last, this)), new_stack)
            }
          | (true, None)
          | (false, _) =>
            inner(rest, tree, stack->Stack.push(this))
          }
        }
      }
    }
    // push the first token to stack to init
    let (first, rest) = switch xs->tokenize->List.fromArray {
    | list{} => raise(ParseError("empty input"))
    | list{this, ...rest} => (this, rest)
    }
    inner(rest, Empty, [first])
  }
}

let parse = data =>
  data->splitNewline->FP_Utils.flatMapArray(FP_Utils.compose(Js.String2.trim, Utils.splitChars))

let examples = () => {
  let parent = ParseTree.makeNodeFromStr("(", 1, ")", 2)
  let t =
    parent
    ->ParseTree.add(ParseTree.makeNodeFromStr("[", 3, "]", 4))
    ->ParseTree.add(ParseTree.makeNodeFromStr("<", 5, ">", 6))

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
}

let solvePart1 = data => {
  data->parse->Js.log2("data")
  let (tree, stack) = data->parse->ParseTree.makeParseTree

  Js.log("tree ---")
  tree->ParseTree.toString->Js.log2("tree")
  Js.log("tree ---")

  //  tree
  //  ->ParseTree.map(x => {
  //    x->ParseTree.toString->Js.log2("tree")
  //    x
  //  })
  //  ->ignore

  stack->Js.log2("stack")

  1
}

let solvePart2 = data => {
  data->ignore
  2
}
