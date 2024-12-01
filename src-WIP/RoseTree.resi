@@ocaml.text(" Simple implementation of multi-way trees aka Rose Trees. ")

type rec tree<'a> = Node(('a, list<tree<'a>>))

@ocaml.doc(" [make ~f seeds] builds a tree from the projection function and a
    list of seed values. ")
let make: (~f: 'a => ('b, list<'a>), 'a) => tree<'b>

@ocaml.doc(" [draw ~f tree] returns a neat two dimentional drawing of tree;
    shamelessly ripped of from Haskell 'containers' package. ")
let draw: (~f: 'a => string, tree<'a>) => list<string>

@ocaml.doc(" [map ~f tree] map a function over each node of the [tree] and return
    a new tree with the results, returned by [f]. ")
let map: (~f: 'a => 'b, tree<'a>) => tree<'b>

@ocaml.doc(" [flatten tree] returns a list of elements of a tree in a pre-order. ")
let flatten: tree<'a> => list<'a>
