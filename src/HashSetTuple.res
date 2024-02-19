@@uncurried
@@uncurried.swap

module Tuple = {
  type t = (string, int)
  let eq = (. (s1, _): t, (s2, _): t) => s1 === s2

  type seed = int

  // from belt_HashSetString.ml
  @val external caml_hash_mix_string: (seed, string) => seed = "caml_hash_mix_string"
  @val external final_mix: seed => seed = "caml_hash_final_mix"

  let hash = (. (s, _): t) => final_mix(caml_hash_mix_string(0, s))
}

module HashSetTupleCmp = Belt.Id.MakeHashable(Tuple)

let make = (~hintSize: int) => Belt.HashSet.make(~hintSize, ~id=module(HashSetTupleCmp))
