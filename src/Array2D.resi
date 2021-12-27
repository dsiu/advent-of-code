type t<'a> = array<array<'a>>

let make: ((int, int), 'a) => t<'a>

let copy: t<'a> => t<'a>

let lengthX: t<'a> => int

let lengthY: t<'a> => int

let isValidXY: (t<'a>, (int, int)) => bool

let set: (t<'a>, (int, int), 'a) => bool

let setYEquals: (t<'a>, int, array<'a>) => bool

let get: (t<'a>, (int, int)) => option<'a>

let getExn: (t<'a>, (int, int)) => 'a

let getXEquals: (t<'a>, int) => option<array<'a>>

let getYEquals: (t<'a>, int) => option<array<'a>>

//let keep: (t<'a>, 'a => bool) => t<'a>

let map: (t<'a>, 'a => 'b) => t<'b>

let mapWithIndex: (t<'a>, ((int, int), 'a) => 'b) => t<'b>

let flatten: t<'a> => array<'a>

let crop: (t<'a>, (int, int), ~len_x: int, ~len_y: int) => t<'a>

let eq: (t<'a>, t<'a>) => bool
