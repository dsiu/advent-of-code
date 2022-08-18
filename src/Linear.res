module V2 = {
  type t<'a> = ('a, 'a)
  let cmp = ((a, b), (c, d)) =>
    switch Pervasives.compare(a, c) {
    | 0 => Pervasives.compare(b, d)
    | r => r
    }
}

module V3 = {
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
}
