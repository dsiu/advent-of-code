// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";

function push(queue, priority, element, cmp) {
  if (typeof queue !== "object") {
    return {
      TAG: "Node",
      _0: priority,
      _1: element,
      _2: "Empty",
      _3: "Empty"
    };
  }
  let right = queue._3;
  let left = queue._2;
  let e = queue._1;
  let p = queue._0;
  if (cmp(priority, p)) {
    return {
      TAG: "Node",
      _0: priority,
      _1: element,
      _2: push(right, p, e, cmp),
      _3: left
    };
  } else {
    return {
      TAG: "Node",
      _0: p,
      _1: e,
      _2: push(right, priority, element, cmp),
      _3: left
    };
  }
}

function remove_top(queue, cmp) {
  if (typeof queue !== "object") {
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  }
  let left = queue._2;
  let tmp = queue._3;
  if (typeof tmp !== "object") {
    return left;
  }
  if (typeof left !== "object") {
    return queue._3;
  }
  let right = queue._3;
  let rPriority = right._0;
  let lPriority = left._0;
  if (cmp(lPriority, rPriority)) {
    return {
      TAG: "Node",
      _0: lPriority,
      _1: left._1,
      _2: remove_top(left, cmp),
      _3: right
    };
  } else {
    return {
      TAG: "Node",
      _0: rPriority,
      _1: right._1,
      _2: left,
      _3: remove_top(right, cmp)
    };
  }
}

function pop(queue, cmp) {
  if (typeof queue === "object") {
    return [
      queue._0,
      queue._1,
      remove_top(queue, cmp)
    ];
  }
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

let Impl = {
  empty: "Empty",
  push: push,
  remove_top: remove_top,
  pop: pop
};

let cmp = Primitive_object.lessequal;

function push$1(queue, priority, element) {
  return push(queue, priority, element, cmp);
}

function remove_top$1(queue) {
  return remove_top(queue, cmp);
}

function pop$1(queue) {
  return pop(queue, cmp);
}

let MinPriorityQueue = {
  cmp: cmp,
  empty: "Empty",
  push: push$1,
  remove_top: remove_top$1,
  pop: pop$1
};

let cmp$1 = Primitive_object.greaterequal;

function push$2(queue, priority, element) {
  return push(queue, priority, element, cmp$1);
}

function remove_top$2(queue) {
  return remove_top(queue, cmp$1);
}

function pop$2(queue) {
  return pop(queue, cmp$1);
}

let MaxPriorityQueue = {
  cmp: cmp$1,
  empty: "Empty",
  push: push$2,
  remove_top: remove_top$2,
  pop: pop$2
};

export {
  Impl,
  MinPriorityQueue,
  MaxPriorityQueue,
}
/* No side effect */