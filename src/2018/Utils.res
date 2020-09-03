open Belt

let map_string_dump = m => Map.String.forEach(m,
  (k, v) => {
    Js.Console.log(j`key:${k}, val:${string_of_int(v)}`)
  }
);

 let map_int_int_dump = m => MutableMap.Int.forEach(m,
  (k, v) => {
    Js.Console.log(j`key:${k->string_of_int}, val:${v->string_of_int}`)
  }
  );

let flattenArray = (arr: array<array<'a>>) : array<'a> =>
  arr
    ->Array.reduce([], (acc, x) => acc->Array.concat(x))
