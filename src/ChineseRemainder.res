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
      aa.contents = bb.contents
      bb.contents = c

      let tmp = x0.contents
      x0.contents = x1.contents - q * x0.contents
      x1.contents = tmp
    }
    if x1.contents < 0 {
      x1.contents = x1.contents + b0
    }
    x1.contents
  }
}

let crt = (rem, num) => {
  let sum = ref(0)
  let prod = num->Array.reduce(1, (a, c) => a * c)
  for i in 0 to num->Array.length - 1 {
    let (ni, ri) = (num[i]->Option.getExn, rem[i]->Option.getExn)
    let p = Js.Math.floor_int(prod->Js.Int.toFloat /. ni->Js.Int.toFloat)

    sum.contents = sum.contents + ri * p * mulInv(p, ni)
  }

  mod(sum.contents, prod)
}

open ReScriptJs.Js

let big_zero = BigInt.fromInt(0)
let big_one = BigInt.fromInt(1)

let mulInvBigInt = (a: BigInt.t, b: BigInt.t) => {
  let b0 = b
  let (x0, x1) = (ref(big_zero), ref(big_one))

  if b === big_one {
    big_one
  } else {
    let (aa, bb) = (ref(a), ref(b))

    // need to coerce to prevent using caml
    while %raw(`aa > big_one`) {
      let q = BigInt.div(aa.contents, bb.contents)
      let c = BigInt.mod(aa.contents, bb.contents)

      aa.contents = bb.contents
      bb.contents = c

      let tmp = x0.contents
      x0.contents = BigInt.sub(x1.contents, BigInt.mul(q, x0.contents))
      x1.contents = tmp
    }
    // need to coerce to prevent using caml
    if %raw(`x1 < big_zero`) {
      x1.contents = BigInt.add(x1.contents, b0)
    }
    x1.contents
  }
}

let crtBigInt = (rem, num) => {
  let sum = ref(big_zero)
  let prod = num->Belt.Array.reduce(big_one, (a, c) => BigInt.mul(a, c))

  for i in 0 to num->Belt.Array.length - 1 {
    let (ni, ri) = (num[i]->Option.getExn, rem[i]->Option.getExn)
    let p = BigInt.div(prod, ni)

    sum.contents = BigInt.add(sum.contents, BigInt.mul(BigInt.mul(ri, p), mulInvBigInt(p, ni)))
  }

  BigInt.mod(sum.contents, prod)
}
