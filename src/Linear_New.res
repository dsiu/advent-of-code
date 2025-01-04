module type V_S = {
  type t // container
  let cmp: (t, t) => int
  let add: (t, t) => t
}

module type V2_S = {
  type e
  type t = (e, e)
  include V_S with type t := t

  let make: (e, e) => t
}

module type V3_S = {
  type e
  type t = (e, e, e)
  include V_S with type t := t

  let make: (e, e, e) => t
}

module type CMP = {
  type t
  let cmp: (t, t) => int
}

module type ADD = {
  type t
  let add: (t, t) => t
}

module type CMP_ADD = {
  type t
  include CMP with type t := t
  include ADD with type t := t
}

module IntElem = {
  type t = int
  let cmp = (a, b) => a - b
  let add = (a, b) => a + b
}

module FloatElem = {
  type t = float
  let cmp = (a, b) => Float.toInt(a -. b)
  let add = (a, b) => a +. b
}

module Make_V2: (E: CMP_ADD) => (V2_S with type e := E.t) = (E: CMP_ADD) => {
  type e = E.t
  type t = (e, e)

  let make = (a, b) => (a, b)

  let cmp = ((a, b), (c, d)) => {
    switch E.cmp(a, c) {
    | 0 => E.cmp(b, d)
    | r => r
    }
  }

  let add = ((a, b), (c, d)) => (E.add(a, c), E.add(b, d))
}

module Make_V3: (E: CMP_ADD) => (V3_S with type e := E.t) = (E: CMP_ADD) => {
  type e = E.t
  type t = (e, e, e)

  let make = (a, b, c) => (a, b, c)

  let cmp = ((a, b, c), (d, e, f)) => {
    switch E.cmp(a, d) {
    | 0 =>
      switch E.cmp(b, e) {
      | 0 => E.cmp(c, f)
      | r => r
      }

    | r => r
    }
  }

  let add = ((a, b, c), (d, e, f)) => (E.add(a, d), E.add(b, e), E.add(c, f))
}

module V2_Int = Make_V2(IntElem)

module V2_Float = Make_V2(FloatElem)

module V3_Int = Make_V3(IntElem)
