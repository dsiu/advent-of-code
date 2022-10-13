//
// https://work.njae.me.uk/2020/12/25/advent-of-code-2020-day-16/
//

open Belt
open Utils
open! FP_Utils

let log = Js.Console.log
let log2 = Js.Console.log2

type range = Range(int, int) // a Range is two limits.
type body = Body(range, range) // A Body is two Ranges
type ruleSet = RuleSet(Map.String.t<body>) //A RuleSet is a Map of rule names (Strings) to Bodies.
type ticket = array<int> // Ticket is a list of Ints

let rangeToString = (Range(a, b)) => a->Int.toString ++ "-" ++ b->Int.toString
let bodyToString = (Body(r1, r2)) => r1->rangeToString ++ " or " ++ r2->rangeToString
let inRange = (Range(lower, upper), value) => lower <= value && value <= upper
let matchesRule = (Body(a, b), value) => inRange(a, value) || inRange(b, value)

let validForAnyField = (RuleSet(rules), value) => {
  open Array
  some(rules->Map.String.valuesToArray, matchesRule(_, value))
}

let ticketErrorRate = (rules, tickets: array<ticket>) => {
  open Array
  sumIntArray(tickets->map(t => t->keep(v => !validForAnyField(rules, v)))->flatten)
}

let part1 = ticketErrorRate

/**
  A ticket is valid if every value is valid for some rule.
  */
let isValidTicket = (RuleSet(rules), ticket) => {
  open Array
  every(ticket->map(validForAnyField(RuleSet(rules), _)), identity)
}

/**
  A column is a possible match for a rule if all values in that column match the rule. I can find
  all the candidate column indexes for a rule by map'ing that test over all the columns (zipped with
  the indexes).
*/
let possibleColumns = (ticketCols, body) => {
  open Array
  let columnMatches = ((_, col)) => {
    every(col, matchesRule(body, _))
  }

  let idx = makeBy(ticketCols->length, identity)
  zip(idx, ticketCols)->keep(columnMatches)->map(fst)
}

/**
  I find all the columns that match for all the rules by mapping possibleColumns over rule. This is
  where I filter out the invalid tickets, and transpose them to have a list of columns rather than a
  list of tickets.
*/
type colCandidateSet = ColCandidateSet(Map.String.t<array<int>>)

let possibleColumnsAll = (RuleSet(rules), tickets: array<ticket>) => {
  open Array
  let validTickets = tickets->keep(isValidTicket(RuleSet(rules)))
  let ticketCols = Utils.transpose(validTickets)

  rules->Map.String.map(possibleColumns(ticketCols, _))
}

/**
  my own CSP
*/
let reduceCandidate = (ColCandidateSet(candidates)) => {
  let findNextCandidate = (ColCandidateSet(candidates)) => {
    let only1Elem = (_, v) => v->Array.length == 1
    candidates->Map.String.findFirstBy(only1Elem)
  }

  let rec inner = (ColCandidateSet(c), solved: array<(string, int)>) => {
    switch ColCandidateSet(c)->findNextCandidate {
    | Some((k, v)) => {
        let v' = v->Array.getExn(0)
        let solved' = Array.concat(solved, [(k, v')])

        let removeFound = Array.keep(_, x => x != v')

        inner(ColCandidateSet(c->Map.String.remove(k)->Map.String.map(removeFound)), solved')
      }

    | None => solved
    }
  }
  inner(ColCandidateSet(candidates), [])
}

@@warning("-8")
let parse = data => {
  let trim = Js.String2.trim
  let split = Js.String2.split
  let map = Array.map
  let intFromStrEx = Int.fromString->compose(Option.getExn)

  let [rules, my, nearby] = data->splitDoubleNewline->map(x => x->splitNewline->map(trim))

  let parseRange = s => {
    let [a, b] = s->split("-")->Array.map(intFromStrEx)
    Range(a, b)
  }

  let parseRule = s => {
    let [ruleStr, rangesStr] = s->split(": ")
    let [r1, r2] = rangesStr->split(" or ")->Array.map(parseRange)
    (ruleStr, Body(r1, r2))
  }

  let ruleSet = rules->Array.map(parseRule)->Map.String.fromArray->RuleSet

  let parseTicket = s => {
    s->split(",")->map(intFromStrEx)
  }

  let myTicket = my->Array.getExn(1)->parseTicket
  let nearbyTickets = nearby->Array.sliceToEnd(1)->map(parseTicket)

  //  ruleSet->Printable.MapString.toString(bodyToString)->log
  //  myTicket->log
  //  nearbyTickets->log

  (ruleSet, myTicket, nearbyTickets)
}

let solvePart1 = data => {
  let (rules, _myTicket, nearbyTickets) = data->parse
  part1(rules, nearbyTickets)
}

let solvePart2 = data => {
  let (rules, myTicket, nearbyTickets) = data->parse
  //  isValidTicket(rules, myTicket)->log
  //  nearbyTickets->Array.map(isValidTicket(rules, _))->log

  let pc = possibleColumnsAll(rules, nearbyTickets)
  //  pc->Map.String.toArray->log
  let colMapping = ColCandidateSet(pc)->reduceCandidate

  open Array
  colMapping
  ->keepMap(((k, col)) => {
    k->Js.String2.startsWith("departure") ? Some(myTicket->Array.getExn(col)) : None
  })
  ->reduce(1., (acc, x) => Float.fromInt(x) *. acc) // product of array
}
