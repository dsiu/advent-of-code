let log2 = Console.log2

// Min
module PQMin = PriorityQueue.MinPriorityQueue

// Int String
let qIntString = PQMin.empty
let qIntString = qIntString->PQMin.push(3, "a")->PQMin.push(2, "b")->PQMin.push(1, "c")

qIntString->PQMin.pop->log2("qIntString pop")

// Float String
let qFloatString = PQMin.empty
let qFloatString = qFloatString->PQMin.push(1.1, "a")->PQMin.push(1.2, "b")->PQMin.push(1.3, "c")

qFloatString->PQMin.pop->log2("qFloatString pop")

// Bignum, Array<string>
let qBignumString = PQMin.empty
let qBignumString =
  qBignumString->PQMin.push(1n, ["a"])->PQMin.push(2n, ["b"])->PQMin.push(3n, ["c"])

qBignumString->PQMin.pop->log2("qBignumString pop")

// Max
module PQMax = PriorityQueue.MaxPriorityQueue

// Int String
let qIntString = PQMax.empty
let qIntString = qIntString->PQMax.push(3, "a")->PQMax.push(2, "b")->PQMax.push(1, "c")

qIntString->PQMax.pop->log2("qIntString pop")
