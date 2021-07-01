type t<'a> = array<array<'a>>

let make: ((int, int), 'a) => t<'a>

let set: (t<'a>, (int, int), 'a) => bool

let get: (t<'a>, (int, int)) => option<'a>

let keep: (t<'a>, 'a => bool) => t<'a>

let map: (t<'a>, 'a => 'b) => t<'b>
