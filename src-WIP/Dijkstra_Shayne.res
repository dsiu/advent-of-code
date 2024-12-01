//
// https://blog.shaynefletcher.org/2018/05/dijkstras-algorithm.html
//
// Dijkstra's algorithm
// Shayne Fletcher
//

/*
  This article assumes familiarity with Dijkstra's shortest path algorithm. For a refresher, see [1].
  The code assumes open Core is in effect and is online here.
*/

/*
  The first part of the program organizes our thoughts about what we are setting out to compute. The
  signature summarizes the notion (for our purposes) of a graph definition in modular form. A module
  implementing this signature defines a type vertex_t for vertices, a type t for graphs and type
  extern_t : a representation of a t for interaction between an implemening module and its "outside
  world".
*/

module type Graph_sig = {
  type vertex_t
  type t
  type extern_t

  type load_error = [#Duplicate_vertex(vertex_t)]
  exception Load_error(load_error)

  let of_adjacency: extern_t => [#Ok(t) | #Load_error(load_error)]
  let to_adjacency: t => extern_t

  module type Dijkstra = {
    type state

    type error = [#Relax(vertex_t)]
    exception Error(error)

    let dijkstra: (vertex_t, t) => [#Ok(state) | #Error(error)]
    let d: state => array<(vertex_t, float)>
    let shortest_paths: state => array<(vertex_t, array<vertex_t>)>
  }
}

/*
  A realization of Graph_sig provides "conversion" functions of_adjacency/to_adjacency between the types extern_t and t and nests a module Dijkstra. The signature of the sub-module Dijkstra requires concrete modules provide a type state and an implementation of Dijkstra's algorithm in terms of the function signature val dijkstra : vertex_t -> t -> [ `Ok of state | `Error of error ].
  For reusability, the strategy for implementing graphs will be generic programming via functors over modules implementing s vertex type.

  An implementation of the module type GRAPH defines a module type VERT which is required to provide
  a comparable type t. It further defines a module type S that is exactly module type Graph_sig
  above. Lastly, modules of type GRAPH provide a functor Make that maps any module of type VERT to
  new module of type S fixing extern_t to an adjacency list representation in terms of the native
  OCaml type 'a list and float to represent weights on edges.
 */

module type GRAPH = {
  module type VERT = {
    type t
    include Belt.Id.Comparable with type t := t

    let equal: (t, t) => bool
  }

  module type S = {
    include Graph_sig
  }

  module Make: (V: VERT) =>
  {
    include S with type vertex_t = V.t and type extern_t = list<(V.t, list<(V.t, float)>)>
  }
}

/*
  The two module types Graph_sig and GRAPH together provide the specification for the program.
  module Graph in the next section implements this specification.
 */

/*
  Implementation of module Graph is in outline this.
 */

module Graph: GRAPH = {
  module type VERT = {
    type t
    include Belt.Id.Comparable with type t := t

    let equal: (t, t) => bool
  }

  module type S = {
    include Graph_sig
  }

  /*
  As per the requirements of GRAPH the module types VERT and S are provided as is the functor Make.
  It is the code that is ellided by the ... above in the definition of Make that is now the focus.
 */
  module Make: (V: VERT) =>
  {
    include S with type vertex_t = V.t and type extern_t = list<(V.t, array<(V.t, float)>)>
  } = (V: VERT) => {
    /* Modules produced by applications of Make satisfy S. This requires suitable definitions of
     * types vertext_t, t and extern_t. The modules Map and Set are available due to modules of type
     * VERT being comparable in their type t. */
    //    module Map = V.Map
    open Belt
    module Map = Map
    //    module Set = V.Set
    module Set = Set

    type vertex_t = V.t
    type t = Map.t<vertex_t, array<(vertex_t, float)>, vertex_t>
    type extern_t = list<(vertex_t, list<(vertex_t, float)>)>
    type load_error = [#Duplicate_vertex(vertex_t)]
    exception Load_error(load_error)

    /* While the external representation extern_t of graphs is chosen to be an adjacency list
     * representation in terms of association lists, the internal representation t is a vertex map
     * of adjacency lists providing logarithmic loookup complexity. The conversion functions between
     * the two representations "come for free" via module Map. */

    let to_adjacency = g => Map.toArray(g)

    let of_adjacency_exn = l => {
      //      switch Belt.Map.fromArray(l) {
      //      | Belt.Result.Ok(t) => t
      //      | Belt.Result.Error(c) => raise(Load_error(#Duplicate_vertex(c)))
      //      }
      Map.fromArray(l)
    }

    let of_adjacency = l => {
      try {
        #Ok(of_adjacency_exn(l))
      } catch {
      | Load_error(err) => #Load_error(err)
      }
    }

    /* At this point the "scaffolding" for Dijkstra's algorithm, that part of GRAPH dealing with the
     * representation of graphs is implemented. */

    /* The interpretation of Dijkstra's algorithm we adopt is functional : the idea is we loop over
     * vertices relaxing their edges until all shortest paths are known. What we know on any
     * recursive iteration of the loop is a current "state" (of the computation) and each iteration
     * produces a new state. This next definition is the formal definition of type state. */
    module Dijkstra = {
      /**
         The fields of this record are:
         src : vertex_t, the source vertex;
         g : t, G the graph;
         d : float Map.t, d the shortest path weight estimates;
         pre : vertex_t Map.t, π the predecessor relation;
         s : Set.t, the set S of nodes for which the lower bound shortest path weight is known;
         v_s : (vertex_t * float) Heap.t, V - {S}, , the set of nodes of g for which the lower bound of the shortest path weight is not yet known ordered on their estimates.
       */
      type state = {
        src: vertex_t,
        g: t,
        d: Map.t<vertex_t, float, V.identity>,
        pred: Map.t<vertex_t, float, V.identity>,
        s: Set.t<vertex_t, V.identity>,
        v_s: Queue.t<(vertex_t, float)>,
      }

      /**
       Function invocation init src g compuates an initial state for the graph g containing the
       source node src. In the initial state, d is everywhere ∞ except for src which is 0. Set S
       (i.e. s) and the predecessor relation π (i.e. pred) are empty and the set V - {S} (i.e. v_s)
       contains all nodes.
      */
      let init = (src, g) => {
        let init = x =>
          if V.equal(src, x) {
            0.0
          } else {
            RescriptCore.Float.Constants.positiveInfinity
          }
        let d =
          Map.keysToArray(g)->Belt.Array.reduceWithIndex(Map.make(~id=module(V)), (acc, x, _) =>
            Map.set(acc, x, init(x))
          )
        {
          src,
          g,
          s: Set.make(~id=module(V)),
          d,
          pred: Map.make(~id=module(V)),
          v_s: Heap.fromArray(Map.toArray(d), (a, b) => {
            let (_, e1) = a
            let (_, e2) = b
            Js.Math.sign(e1 - e2)
          }),
        }
      }

      /**
        Relaxing an edge (u, v) with weight w (u, v) tests whether the shortest path to v so far can
        be improved by going through u and if so, updating d (v) and π (v) accordingly.
      */
      type error = [#Relax(vertex_t)]
      exception Error(error)

      /**
        Here, relaxation can result in a linear heap update operation. A better implementation might
        seek to avoid that.
*/
      let relax = (state, (u, v, w)) => {
        let {d, pred, v_s, _} = state
        let dv = switch Map.get(v, d) {
        | Some(dv) => dv
        | None => raise(Error(#Relax(v)))
        }
        let du = switch Map.get(u, d) {
        | Some(du) => du
        | None => raise(Error(#Relax(u)))
        }
        if dv > du +. w {
          let dv = du +. w
          switch Heap.find(v_s, (n, _) => V.equal(n, v)) {
          | Some(tok) => ignore(Heap.update(v_s, tok, (v, dv)))
          | None => raise(Error(#Relax(v)))
          }
          {
            ...state,
            d: Map.update(v, _ => Some(dv), d),
            pred: Map.set(v, u, Map.remove(v, pred)),
          }
        } else {
          state
        }
      }

      /**
        One iteration of the body of the loop of Dijkstra's algorithm consists of the node in V -
        {S} with the least shortest path weight estimate being moved to S and its edges relaxed.
     */
      let dijkstra_exn = (src, g) => {
        let rec loop = state => {
          let {s, v_s, _} = state
          if Heap.isEmpty(v_s) {
            state
          } else {
            let u = Heap.pop(v_s)->Option.getExn->fst
            let newState = {...state, s: Set.add(u, s)}
            loop(
              Map.get(u, g)
              ->Option.getExn
              ->Belt.List.reduce(newState, (state, (v, w)) => relax(state, (u, v, w))),
            )
          }
        }
        loop(init(src, g))
      }

      let dijkstra = (src, g) => {
        try {
          #Ok(dijkstra_exn(src, g))
        } catch {
        | Error(err) => #Error(err)
        }
      }

      /**
        The shortest path estimates contained by a value of state is given by the projection d.
        */
      let d = state => Map.toList(state.d)

      /**
        The shortest paths themselves are easily computed as,
      */
      let path = (state, n) => {
        let rec loop = (acc, x) =>
          if V.equal(x, state.src) {
            [x, ...acc]
          } else {
            loop([x, ...acc], Map.getExn(x, state.pred))
          }
        loop([], n)
      }

      let shortest_paths = state => {
        Map.keys(state.g)->Belt.List.map(n => (n, path(state, n)))
      }
    }
  }
}
