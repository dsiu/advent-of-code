open Belt

let map_string_dump = m => Map.String.forEach(m,
  (k, v) => {
    Js.Console.log(j`key:${k}, val:${string_of_int(v)}`)
  }
);
