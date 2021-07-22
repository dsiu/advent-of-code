let data = AOC2018_Day4_Data.data
let sampleData = AOC2018_Day4_Data_Sample.data
open Belt
@@warning("-27-8")
//
// data structures
//

module Attendance = {
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

  let minsSleptPerHourRec = MutableSet.Int.size

  let minsSleptTotal = dr => {
    dr->MutableMap.String.reduce(0, (a, k, hr) => {
      a + hr->minsSleptPerHourRec
    })
  }

  let perGuardMinsSlept = MutableMap.Int.map(_, minsSleptTotal)

  let findLaziestGuard = gAtt => {
    gAtt
    ->perGuardMinsSlept
    ->MutableMap.Int.reduce((-1, -1), (a, k, v) => {
      let (gid, minSlept) = a
      v > minSlept ? (k, v) : a
    })
  }

  let tallySleptPerMin = dr => {
    // Js.Console.log("tallySleptPerMin")
    dr->MutableMap.String.reduce(MutableMap.Int.make(), (a, k, hr) => {
      hr->MutableSet.Int.forEach(m => {
        a->MutableMap.Int.update(m, prev => {
          switch prev {
          | Some(prev) => Some(prev + 1)
          | None => Some(1)
          }
        })
      })
      a
    })
  }

  let perGuardTallySleptPerMin = MutableMap.Int.map(_, tallySleptPerMin)

  let perGuardMostSleptMin = gAtt => {
    gAtt
    ->perGuardTallySleptPerMin
    ->MutableMap.Int.map(t => {
      t->MutableMap.Int.reduce((-1, -1), (a, k, v) => {
        // Js.Console.log(a)
        // Js.Console.log(`k:${k->string_of_int}, v:${v->string_of_int}`)
        let (which_min, how_many) = a
        v > how_many ? (k, v) : a
      })
    })
  }

  let busiestMin = gAtt => {
    gAtt
    ->perGuardMostSleptMin
    ->MutableMap.Int.reduce((-1, (-1, -1)), (a, k, v) => {
      let (guard, busyMin) = a
      let (which_min, how_many) = busyMin
      let (next_which_min, next_how_many) = v
      next_how_many > how_many ? (k, v) : a
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

module Parser = {
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
    | (Some(x), None, None) => Begin(x->parseRegexResult->unboxBeginLine)
    | (None, Some(x), None) => Asleep(x->parseRegexResult->unboxAsleepLine)
    | (None, None, Some(x)) => Awake(x->parseRegexResult->unboxAwakeLine)
    | (_, _, _) => raise(Not_found)
    }
  }

  type rstate =
    | AtInit
    | AtBegin
    | AtAsleep
    | AtAwake

  type rresult = {
    state: rstate,
    gid: int,
    sleptSince: int,
    gAtt: Attendance.t,
  }

  let processLineReducer = (a: rresult, x) => {
    let {state} = a

    switch x->parseLine {
    | Begin(d) => {
        let {raw, date, h, m, gid} = d

        {...a, state: AtBegin, gid: gid, sleptSince: -1}
      }
    | Asleep(d) => {
        assert (state === AtAwake || state === AtBegin)
        let {raw, date, h, m, gid} = d

        {...a, state: AtAsleep, sleptSince: m}
      }
    | Awake(d) => {
        assert (state === AtAsleep)
        let {raw, date, h, m} = d
        let {sleptSince, gAtt, gid} = a

        Attendance.insertGuardRec(gAtt, gid, date, sleptSince, m - 1)
        // insert record to Attendance
        {...a, state: AtAwake, sleptSince: -1}
      }
    }
  }
}

// ==============================

let solvePart1 = data => {
  let sortLines = data->Js.String2.split("\n")->SortArray.String.stableSort
  // sortLines->Js.Console.log

  let initState: Parser.rresult = {
    state: AtInit,
    gid: 0,
    sleptSince: 0,
    gAtt: MutableMap.Int.make(),
  }
  let {gAtt}: Parser.rresult = sortLines->Array.reduce(initState, Parser.processLineReducer)
  // Js.Console.log("=== dump Attendance")
  // gAtt->Attendance.dump

  // Js.Console.log("=== dump perGuardMinsSlept")
  // gAtt->Attendance.perGuardMinsSlept->Utils.map_int_int_dump

  // Js.Console.log("=== dump findLaziestGuard")
  let laziest = gAtt->Attendance.findLaziestGuard
  // laziest->Js.Console.log

  // Js.Console.log("=== dump perGuardTallySleptPerMin")
  // gAtt->Attendance.perGuardTallySleptPerMin->MutableMap.Int.forEach((k, v) => {
  // Js.Console.log(`key:${k->string_of_int}`)
  // v->Utils.map_int_int_dump
  // })

  // Js.Console.log("=== dump perGuardMostSleptMin")
  let laziestMins = gAtt->Attendance.perGuardMostSleptMin
  // laziestMins->MutableMap.Int.forEach((k, v) => {
  // Js.Console.log(`key:${k->string_of_int}`)
  // Js.Console.log(v)
  // })

  // Js.Console.log("=== part1 - dump gid x lazest min")
  let (laziestGid, totalMins) = laziest
  let (laziestMin, how_many) = laziestMins->MutableMap.Int.get(laziestGid)->Option.getExn
  // Js.Console.log(laziestGid)
  // Js.Console.log(laziestMin)
  let part1Answer = laziestGid * laziestMin

  // Js.Console.log(part1Answer)
  part1Answer
}

let solvePart2 = data => {
  // Js.Console.log("=== part2 - dump gid x busy min")
  let sortLines = data->Js.String2.split("\n")->SortArray.String.stableSort
  // sortLines->Js.Console.log

  let initState: Parser.rresult = {
    state: AtInit,
    gid: 0,
    sleptSince: 0,
    gAtt: MutableMap.Int.make(),
  }
  let {gAtt}: Parser.rresult = sortLines->Array.reduce(initState, Parser.processLineReducer)
  let (busy_guy, busy_min) = gAtt->Attendance.busiestMin
  // Js.Console.log(busy_guy)
  let (which_busy_min, how_many) = busy_min

  let part2Answer = busy_guy * which_busy_min

  // Js.Console.log(part2Answer)
  part2Answer
}
// 1217
