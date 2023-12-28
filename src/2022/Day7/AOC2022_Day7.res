open Stdlib
open Utils
let log = Js.Console.log

module A = Array

type parsedObject =
  | CD(string)
  | LS
  | PDirectory(string)
  | PFile(int, string)

let parsedObjectToString = obj => {
  switch obj {
  | CD(dir) => "CD " ++ dir
  | LS => "LS"
  | PDirectory(dir) => "PDirectory " ++ dir
  | PFile(size, file) => "PFile " ++ string_of_int(size) ++ " " ++ file
  }
}

module CmdParser = {
  open ResParser_Utils

  let cmdCD = {
    P.string("$ cd ")
    ->P.andThen(string)
    ->P.map(((_, s2)) => {
      CD(s2)
    })
  }

  let cmdLS = {
    P.string("$ ls")->P.map(_ => LS)
  }

  let outputDir = {
    P.string("dir ")->P.andThen(string)->P.map(((_, s2)) => PDirectory(s2))
  }

  let outputFile = {
    digits
    ->P.andThen(manyWhitespace)
    ->P.andThen(string)
    ->P.map((((digits, _whitespaces), lc)) => {
      PFile(digits->int_of_string, lc)
    })
  }

  type parser = P.t<parsedObject>
  let parser: parser = P.choice([cmdCD, cmdLS, outputDir, outputFile])

  type result = P.parseResult<parsedObject>
  let result: result = P.run(parser, "$ cd /")

  let run = P.run(parser, _)
}

module M = Belt.Map.String
type directory = Dir(string, M.t<int>)
type containedSize = CSize(string, int)
type dTree = Tree(directory)
type zdTree = TreePos
type sTree = Tree(containedSize)

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  data
  ->parse
  ->A.map(CmdParser.run)
  ->A.filterMap(result => {
    switch result {
    | Ok((parsedObject, _state)) => Some(parsedObject->parsedObjectToString)
    | Error(_) => None
    }
  })
  ->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
