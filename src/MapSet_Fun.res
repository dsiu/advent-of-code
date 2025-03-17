let log = Console.log
let log2 = Console.log2

type mapIntString = Map.t<int, string>
type mapStringInt = Map.t<string, int>

let mIS: mapIntString = Map.make()
let mSI: mapStringInt = Map.make()

mIS->Map.set(1, "one")
mSI->Map.set("two", 2)

type setInt = Set.t<int>
type setString = Set.t<string>

let sI: setInt = Set.make()
let sS: setString = Set.make()

sI->Set.add(1)
sS->Set.add("two")

let t = (1, "two")
