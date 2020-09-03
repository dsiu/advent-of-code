let data = Day4_Data.data
let testData = Day4_Data_Test.data
open Belt

//
// data structures
//

module GuardAttendance = {
  // record for mins slept

  type hourRec = MutableSet.Int.t

  // {"1518-08-17": {hourRec}}
  type dayRec = MutableMap.String.t<hourRec>

  // { gid: {dayRecord}, ...}
  type t = MutableMap.Int.t<dayRec>

  // insert hour record
  let insertHourRec = (hr, from_min, to_min) => {
    Range.forEach(from_min, to_min, i => {
      hr->MutableSet.Int.add(i)
    })
    Some(hr)
  }

  // insert day record
  let insertDayRec = (dr, date, from_min, to_min) => {
    dr->MutableMap.String.update(date, hr => {
      switch hr {
      | Some(hr) => hr->insertHourRec(from_min, to_min)
      | None => MutableSet.Int.make()->insertHourRec(from_min, to_min)
      }
    })
    Some(dr)
  }

  // insert a guard record
  let insertGuardRec = (gAtt, gid, date, from_min, to_min) => {
    // Js.Console.log("insertGuardRec")
    // Js.Console.log(
    // `gid:${gid->string_of_int} date:${date} from_min:${from_min->string_of_int} to_min:${to_min->string_of_int}`,
    // )
    gAtt->MutableMap.Int.update(gid, dr => {
      switch dr {
      | Some(dr) => dr->insertDayRec(date, from_min, to_min)
      | None => MutableMap.String.make()->insertDayRec(date, from_min, to_min)
      }
    })
  }

  let minsSleptPerHourRec = hr => hr->MutableSet.Int.size

  let minsSleptTotal = dr => {
    dr->MutableMap.String.reduce(0, (a, k, hr) => {
      a + hr->minsSleptPerHourRec
    })
  }

  let perGuardMinsSlept = gAtt => {
    gAtt->MutableMap.Int.map(minsSleptTotal)
  }

  let findLaziestGuard = gAtt => {
    gAtt->perGuardMinsSlept->MutableMap.Int.reduce((-1, -1), (a, k, v) => {
      let (gid, minSlept) = a
      v > minSlept ? (k, v) : a
    })
  }

  let tallySleptPerMin = dr => {
    Js.Console.log("tallySleptPerMin")
    dr->MutableMap.String.reduce(MutableMap.Int.make(), (a, k, hr) => {
      hr->MutableSet.Int.forEach(m => {
        a->MutableMap.Int.update(m, prev => {
          let t = switch prev {
          | Some(prev) => Some(prev + 1)
          | None => Some(1)
          }
//          Js.Console.log(`m:${m->string_of_int} t:${t->Option.getExn->string_of_int}`)
          t
        })
      })
      a
    })
  }

  let perGuardTallySleptPerMin = gAtt => {
    gAtt->MutableMap.Int.map(tallySleptPerMin)
  }

  let perGuardMostSleptMin = gAtt => {
    Js.Console.log("debug: perGuardMostSleptMin")

    gAtt->perGuardTallySleptPerMin->MutableMap.Int.map(t => {
      let (which, how_many) = t->MutableMap.Int.reduce((-1, -1), (a, k, v) => {
        Js.Console.log(a)
        Js.Console.log(`k:${k->string_of_int}, v:${v->string_of_int}`)
        let (which, how_many) = a
        v > how_many ? (k,v) : a
      })
      which
    })
  }

  let dump = gAtt => {
    gAtt->MutableMap.Int.forEach((gid, drs) => {
      Js.Console.log(`gid:${gid->string_of_int}`)
      drs->MutableMap.String.forEach((date, hr) => {
        Js.Console.log(`  date:${date}`)
        Js.Console.log(`    Set Size:${hr->MutableSet.Int.size->string_of_int}`)
        hr->MutableSet.Int.toArray->Js.Console.log
      })
    })
  }
}

// guardIds -> date -> wake/sleep
//
let guardBeginsRe = %re("/\[(.*)\s+(\d\d):(\d\d)\]\s+Guard\s+#(\d+)\s+begins shift/i")
let guardAsleepRe = %re("/\[(.*)\s+(\d\d):(\d\d)\]\s+falls asleep/i")
let guardWakeRe = %re("/\[(.*)\s+(\d\d):(\d\d)\]\s+wakes up/i")

let parseRegexResult = r => {
  Js.Re.captures(r)->Array.map(x => Js.Nullable.toOption(x)->Option.getExn)
}

type recType<'a> =
  | Begin('a)
  | Asleep('a)
  | Awake('a)

type lineRec = {
  raw: string,
  date: string,
  h: int,
  m: int,
  gid: int,
}

let unboxBeginLine = l => {
  let [raw, date, h, m, gid] = l
  {raw: raw, date: date, h: h->int_of_string, m: m->int_of_string, gid: gid->int_of_string}
}

let unboxAsleepLine = l => {
  let [raw, date, h, m] = l
  {raw: raw, date: date, h: h->int_of_string, m: m->int_of_string, gid: -1}
}

let unboxAwakeLine = l => {
  let [raw, date, h, m] = l
  {raw: raw, date: date, h: h->int_of_string, m: m->int_of_string, gid: -1}
}

let parseLine = l => {
  let trimmed = l->Js.String.trim
  switch (
    trimmed |> Js.Re.exec_(guardBeginsRe),
    trimmed |> Js.Re.exec_(guardAsleepRe),
    trimmed |> Js.Re.exec_(guardWakeRe),
  ) {
  | (Some(x), None, None) => Begin(parseRegexResult(x) |> unboxBeginLine)
  | (None, Some(x), None) => Asleep(parseRegexResult(x) |> unboxAsleepLine)
  | (None, None, Some(x)) => Awake(parseRegexResult(x) |> unboxAwakeLine)
  | (_, _, _) => raise(Not_found)
  }
}

type rstate =
  | AtBegin
  | AtAsleep
  | AtAwake

type rresult = {
  state: rstate,
  gid: int,
  sleptSince: int,
  gAtt: GuardAttendance.t,
}

let processBegin = (a: rresult, lr: lineRec) => {
  // Js.Console.log("Begin")
  // Js.Console.log(lr)
  let {raw, date, h, m, gid} = lr
  {...a, state: AtBegin, gid: gid, sleptSince: -1}
}

let processAsleep = (a: rresult, lr) => {
  // Js.Console.log("Asleep")
  // Js.Console.log(lr)
  let {raw, date, h, m, gid} = lr
  {...a, state: AtAsleep, sleptSince: m}
}

let processAwake = (a: rresult, lr) => {
  // Js.Console.log("Awake")
  // Js.Console.log(lr)
  let {raw, date, h, m} = lr
  let {sleptSince, gAtt, gid} = a

  GuardAttendance.insertGuardRec(gAtt, gid, date, sleptSince, m - 1)
  // insert record to GuardAttendance
  {...a, state: AtAwake, sleptSince: -1}
}

let parseRecReducer = (a, x) => {
  let lr = parseLine(x)

  switch lr {
  | Begin(d) => processBegin(a, d)
  | Asleep(d) => processAsleep(a, d)
  | Awake(d) => processAwake(a, d)
  }
}

// ==============================

let sortLines = data->Js.String2.split("\n")->SortArray.String.stableSort
sortLines->Js.Console.log
// sortLines->Array.map(parseLine)->Js.Console.log

let initState = {state: AtBegin, gid: 0, sleptSince: 0, gAtt: MutableMap.Int.make()}
let {gAtt} = sortLines->Array.reduce(initState, parseRecReducer)
Js.Console.log("=== dump GuardAttendance")
gAtt->GuardAttendance.dump

Js.Console.log("=== dump perGuardMinsSlept")
gAtt->GuardAttendance.perGuardMinsSlept->Utils.map_int_int_dump

Js.Console.log("=== dump findLaziestGuard")
let laziest = gAtt->GuardAttendance.findLaziestGuard
laziest->Js.Console.log

Js.Console.log("=== dump perGuardTallySleptPerMin")
gAtt->GuardAttendance.perGuardTallySleptPerMin->MutableMap.Int.forEach((k, v) => {
  Js.Console.log(`key:${k->string_of_int}`)
  v->Utils.map_int_int_dump
})

Js.Console.log("=== dump perGuardMostSleptMin")
let laziestMins = gAtt->GuardAttendance.perGuardMostSleptMin
laziestMins->Utils.map_int_int_dump

Js.Console.log("=== dump gid x lazest min")
let (laziestGid, totalMins) = laziest
let laziestMin = laziestMins->MutableMap.Int.get(laziestGid)
Js.Console.log(laziestGid)
Js.Console.log(laziestMin->Option.getExn)
Js.Console.log(laziestGid * laziestMin->Option.getExn)
// 1217
