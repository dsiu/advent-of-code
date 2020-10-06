//open Belt
//open Utils
open Day6
let log = Js.Console.log
let data = Day6_Data.data
let testData = Day6_Data_Test.data


let map = testData->Js.String2.split("\n")->Coord.parseCoords->Day6.Map.make
let areas = map->Day6.Map.findAreas
" ========= "->log
map->Day6.Map.dump
//areas->dump_mapInt_of_int
//areas->Day6.Map.getMaxArea->log

