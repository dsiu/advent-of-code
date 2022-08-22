module type Type = {
  type t<'a>
  let cmp: (t<'a>, t<'a>) => int

  // functor
  let map: (t<'a>, 'a => 'b) => t<'b>

  // monoid
  //  let empty: t<'a>
  //  let append: (t<'a>, t<'a>) => t<'a>
}

module type ElemType = {
  type t
}

module V2: Type with type t<'a> = ('a, 'a) = {
  type t<'a> = ('a, 'a)

  let cmp = ((a, b), (c, d)) =>
    switch Pervasives.compare(a, c) {
    | 0 => Pervasives.compare(b, d)
    | r => r
    }

  let map = ((a, b), f) => (f(a), f(b))
}

module V3: Type with type t<'a> = ('a, 'a, 'a) = {
  type t<'a> = ('a, 'a, 'a)

  let cmp = ((a, b, c), (d, e, f)) =>
    switch Pervasives.compare(a, d) {
    | 0 =>
      switch Pervasives.compare(b, e) {
      | 0 => Pervasives.compare(c, f)
      | r => r
      }
    | r => r
    }

  let map = ((a, b, c), f) => (f(a), f(b), f(c))
}
