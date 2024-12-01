open StdLabels

type rec tree<'a> = Node(('a, list<tree<'a>>))

let rec make = (~f, init) => {
  let (label, forest) = f(init)
  Node(label, List.map(~f=make(~f, ...), forest))
}

let shift = (start, other) => {
  let rec inner = (acc, x) =>
    switch x {
    | list{} => List.rev(acc)
    | list{l, ...lines} =>
      let markup = if acc == list{} {
        start
      } else {
        other
      }
      inner(list{String.concat(~sep="", list{markup, l}), ...acc}, lines)
    }
  inner(list{}, ...)
}

let rec draw = (~f, Node(label, forest)) => {
  let rec inner = x =>
    switch x {
    | list{} => list{}
    | list{t, ...ts} =>
      let (start, other) = switch ts {
      | list{} => ("`- ", "   ")
      | _ => ("+- ", "|  ")
      }

      let lines = draw(~f, t)
      Belt.List.concat(list{"|", ...shift(start, other)(lines)}, inner(ts))
    }
  list{f(label), ...inner(forest)}
}

let flatten = t => {
  let rec inner = (Node(label, forest), acc) => list{
    label,
    ...List.fold_right(~f=inner, ~init=acc, forest),
  }
  inner(t, list{})
}

let rec map = (~f, Node(label, forest)) => Node(f(label), List.map(~f=map(~f, ...), forest))
