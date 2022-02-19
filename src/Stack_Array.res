open Belt

type t<'a> = array<'a>

let push = (t, e) => Array.concat([e], t)

let pop = t => (t[0], t->Array.length >= 1 ? Array.sliceToEnd(t, 1) : [])

let peek = t => t[0]
