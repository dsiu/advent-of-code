open Day5
open Belt

let result = charList->defuse->List.toArray->Js.Array2.joinWith(_, "")
log(result)

// Day5_Data.result -> Js.String2.length -> log

