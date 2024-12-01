//
// Priority Queue
//
// ref: https://www.reddit.com/r/rescript/comments/r3rtu9/comment/hmeuvjf/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
//

module Impl = {
  type rec queue<'p, 'a> = Empty | Node('p, 'a, queue<'p, 'a>, queue<'p, 'a>)

  let empty = Empty

  let rec push = (queue, priority, element, cmp) => {
    switch queue {
    | Empty => Node(priority, element, Empty, Empty)
    | Node(p, e, left, right) =>
      if cmp(priority, p) {
        Node(priority, element, push(right, p, e, cmp), left)
      } else {
        Node(p, e, push(right, priority, element, cmp), left)
      }
    }
  }

  let rec remove_top = (queue, cmp) => {
    switch queue {
    | Empty => raise(Not_found)
    | Node(_, _, left, Empty) => left
    | Node(_, _, Empty, right) => right
    | Node(
        _,
        _,
        Node(lPriority, lElement, _, _) as left,
        Node(rPriority, rElement, _, _) as right,
      ) =>
      if cmp(lPriority, rPriority) {
        Node(lPriority, lElement, remove_top(left, cmp), right)
      } else {
        Node(rPriority, rElement, left, remove_top(right, cmp))
      }
    }
  }

  let pop = (queue, cmp) => {
    switch queue {
    | Empty => raise(Not_found)
    | Node(priority, element, _, _) as queue => (priority, element, remove_top(queue, cmp))
    }
  }
}

module MinPriorityQueue = {
  include Impl

  let empty = Empty
  let cmp = \"<="
  let empty = empty
  let push = (queue, priority, element) => push(queue, priority, element, cmp)
  let remove_top = queue => remove_top(queue, cmp)
  let pop = queue => pop(queue, cmp)
}

module MaxPriorityQueue = {
  include Impl

  let empty = Empty
  let cmp = \">="
  let empty = empty
  let push = (queue, priority, element) => push(queue, priority, element, cmp)
  let remove_top = queue => remove_top(queue, cmp)
  let pop = queue => pop(queue, cmp)
}
