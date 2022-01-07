//
// ReScript wrapper for js-debug https://github.com/debug-js/debug
//

module type Logger = {
  let debug: string => unit
  //  let debug2: (string, 'a) => unit
}

module JSD = {
  type t = string => unit
  @module("debug") external createDebug: string => t = "default"

  type t2<'a> = (. string, Js.t<'a>) => unit
  @module("debug") external createDebug2: string => t2<'a> = "default"

  let debug = (. t, s) => t(s)
  let debug2 = (. t2, s, o) => t2(s, o)
  //  let debug0 = t => t()
}

let make = (moduleName: string): module(Logger) =>
  module(
    {
      let jsd = JSD.createDebug(moduleName)
      //      let jsd2 = JSD.createDebug2(moduleName)

      let debug = message => {
        JSD.debug(. jsd, message)
      }

      //      let debug2 = (message, str: 'a) => {
      //        JSD.debug2(. jsd2, message, str)
      //      }
    }
  )
