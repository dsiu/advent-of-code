//
// https://rosettacode.org/wiki/Chinese_remainder_theorem#JavaScript
//
open Belt

let mulInv = (a, b) => {
  let b0 = b
  let (x0, x1) = (ref(0), ref(1))

  if b === 1 {
    1
  } else {
    let (aa, bb) = (ref(a), ref(b))
    while aa.contents > 1 {
      let q = Js.Math.floor_int(aa.contents->Js.Int.toFloat /. bb.contents->Js.Int.toFloat)

      let c = mod(aa.contents, bb.contents)
      aa := bb.contents
      bb := c

      let tmp = x0.contents
      x0 := x1.contents - q * x0.contents
      x1 := tmp
    }
    if x1.contents < 0 {
      x1 := x1.contents + b0
    }
    x1.contents
  }
}

//
// Suppose we have a system of congruences:
//    x = 5 mod 4
//    x = 3 mod 5
//    x = 7 mod 11
//   The solution is: ' + crt([5,3,7], [4,5,11]
//
let crt = (rem, num) => {
  let sum = ref(0)
  let prod = num->Array.reduce(1, (a, c) => a * c)
  for i in 0 to num->Array.length - 1 {
    let (ni, ri) = (num[i]->Option.getExn, rem[i]->Option.getExn)
    let p = Js.Math.floor_int(prod->Js.Int.toFloat /. ni->Js.Int.toFloat)

    sum := sum.contents + ri * p * mulInv(p, ni)
  }

  mod(sum.contents, prod)
}

//
// BigInt version of crt
//
let big_zero = ReScriptJs.Js.BigInt.fromInt(0)
let big_one = ReScriptJs.Js.BigInt.fromInt(1)
let add = ReScriptJs.Js.BigInt.add
let sub = ReScriptJs.Js.BigInt.sub
let mul = ReScriptJs.Js.BigInt.mul
let div = ReScriptJs.Js.BigInt.div
let mod = ReScriptJs.Js.BigInt.mod

let mulInvBigInt = (a: ReScriptJs.Js.BigInt.t, b: ReScriptJs.Js.BigInt.t) => {
  let b0 = b
  let (x0, x1) = (ref(big_zero), ref(big_one))

  if b === big_one {
    big_one
  } else {
    let (aa, bb) = (ref(a), ref(b))

    // need to coerce to prevent using caml
    while %raw(`aa > big_one`) {
      let q = div(aa.contents, bb.contents)
      let c = mod(aa.contents, bb.contents)

      aa := bb.contents
      bb := c

      let tmp = x0.contents
      x0 := sub(x1.contents, mul(q, x0.contents))
      x1 := tmp
    }
    // need to coerce to prevent using caml
    if %raw(`x1 < big_zero`) {
      x1 := add(x1.contents, b0)
    }
    x1.contents
  }
}

let crtBigInt = (rem, num) => {
  let sum = ref(big_zero)
  let prod = num->Array.reduce(big_one, (a, c) => mul(a, c))

  for i in 0 to num->Array.length - 1 {
    let (ni, ri) = (num[i]->Option.getExn, rem[i]->Option.getExn)
    let p = div(prod, ni)

    sum := add(sum.contents, mul(mul(ri, p), mulInvBigInt(p, ni)))
  }

  mod(sum.contents, prod)
}
