let data = Day4_Data.data
open Belt

// guardIds -> date -> wake/sleep
//
let claimRe = %re("/#(\d+)\s+@\s+(\d+),(\d+):\s(\d+)x(\d+)/i")

let guardBeginsRe = %re("/\[(.*)\]\s+Guard\s+#(\d+)\s+begins shift/i")
let guardAsleepRe = %re("/\[(.*)\]\s+falls asleep/i")
let guardWakeRe = %re("/\[(.*)\]\s+wakes up/i")

let parseRegexResult = r => {
  Js.Re.captures(r)->Array.map(x => Js.Nullable.toOption(x)->Option.getExn)
}

let parseLine = l => {
  let trimmed = l->Js.String.trim
  switch (
    trimmed |> Js.Re.exec_(guardBeginsRe),
    trimmed |> Js.Re.exec_(guardAsleepRe),
    trimmed |> Js.Re.exec_(guardWakeRe),
  ) {
  | (Some(x), None, None) => parseRegexResult(x)
  | (None, Some(x), None) => parseRegexResult(x)
  | (None, None, Some(x)) => parseRegexResult(x)
  | (_, _, _) => raise(Not_found)
  }
}

let sortLines = data->Js.String2.split("\n")->SortArray.String.stableSort

sortLines->Array.map(parseLine)->Js.Console.log
