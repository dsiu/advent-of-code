let log = Js.Console.log

// NOT WORKING
//let n = -1
//n->lsr(0)->log

//open Js.TypedArray2
//let s = [-1, -4294967295]
//let sa = Uint32Array.make(s)
//sa->Uint32Array.unsafe_get(0)->log
//sa->Uint32Array.unsafe_get(1)->log

//let dv = DataView.fromBuffer(sa->Uint32Array.buffer)
//dv->DataView.getUint32(0)->log
//dv->DataView.getUint32(1)->log

//[-1, 4294967295, 1, -4294967295]->Belt.Array.forEach(x => {x->Utils.int32ToUint32->log})
//
