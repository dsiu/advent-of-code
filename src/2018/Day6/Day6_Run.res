open Belt
open Utils
open Day6
let log = Js.Console.log
let data = Day6_Data.data
let testData = Day6_Data_Test.data


let result = testData->Js.String2.split("\n")->Array.map(parse)->Day6.Map.make
result->Day6.Map.dump

