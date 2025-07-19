module PriorityQueue = struct
  (* Priority queue implemented as a min-heap *)
  type 'a t = Empty | Node of 'a * float * 'a t * 'a t

  let empty = Empty

  let rec insert queue x priority =
    match queue with
    | Empty -> Node(x, priority, Empty, Empty)
    | Node(y, p, left, right) ->
        if priority < p then
          Node(x, priority, insert right y p, left)
        else
          Node(y, p, insert right x priority, left)

  let rec extract_min = function
    | Empty -> None
    | Node(x, p, left, Empty) -> Some (x, p, left)
    | Node(x, p, Empty, right) -> Some (x, p, right)
    | Node(x, p, left, right) ->
        match (extract_min left, extract_min right) with
        | (Some (x_l, p_l, left'), Some (x_r, p_r, right')) ->
            if p_l <= p_r then
              Some (x, p, Node(x_l, p_l, left', right))
            else
              Some (x, p, Node(x_r, p_r, left, right'))
        | _ -> failwith "Impossible case in extract_min"

  let pop queue =
    match extract_min queue with
    | None -> (None, Empty)
    | Some (x, _, queue') -> (Some x, queue')

  let is_empty = function
    | Empty -> true
    | _ -> false
end

module AStar = struct
  module NodeMap = Map.Make(struct
    type t = int * int
    let compare = compare
  end)

  module NodeSet = Set.Make(struct
    type t = int * int
    let compare = compare
  end)

  type node = int * int
  type g_score = float NodeMap.t
  type f_score = float NodeMap.t
  type came_from = node NodeMap.t
  type open_set = node PriorityQueue.t * NodeSet.t  (* Queue + Set to check membership *)

  let infinity = Float.infinity

  (* Helper function to add a node to the open set *)
  let add_to_open_set (queue, set) node f_value =
    (PriorityQueue.insert queue node f_value, NodeSet.add node set)

  (* Helper function to check if a node is in the open set *)
  let is_in_open_set (_, set) node =
    NodeSet.mem node set

  (* Helper function to remove a node from the open set *)
  let remove_from_open_set (queue, set) =
    let (node_opt, queue') = PriorityQueue.pop queue in
    match node_opt with
    | None -> (None, (queue', set))
    | Some node -> (Some node, (queue', NodeSet.remove node set))

  (* Reconstruct the path from the came_from map *)
  let rec reconstruct_path came_from current =
    match NodeMap.find_opt current came_from with
    | None -> [current]
    | Some prev -> (reconstruct_path came_from prev) @ [current]

  (* Main A* algorithm *)
  let astar start goal neighbors cost heuristic =
    (* Initial state *)
    let initial_open_set = add_to_open_set (PriorityQueue.empty, NodeSet.empty) start (heuristic start goal) in
    let initial_g_score = NodeMap.add start 0.0 NodeMap.empty in
    let initial_f_score = NodeMap.add start (heuristic start goal) NodeMap.empty in
    let initial_came_from = NodeMap.empty in
    let initial_closed_set = NodeSet.empty in

    (* Process a single neighbor *)
    let process_neighbor current (open_set, closed_set, g_score, f_score, came_from) neighbor =
      if NodeSet.mem neighbor closed_set then
        (open_set, closed_set, g_score, f_score, came_from)
      else
        let current_g = NodeMap.find_opt current g_score |> Option.value ~default:infinity in
        let tentative_g = current_g +. cost current neighbor in
        let neighbor_g = NodeMap.find_opt neighbor g_score |> Option.value ~default:infinity in

        if tentative_g < neighbor_g then
          let new_g_score = NodeMap.add neighbor tentative_g g_score in
          let f_value = tentative_g +. heuristic neighbor goal in
          let new_f_score = NodeMap.add neighbor f_value f_score in
          let new_came_from = NodeMap.add neighbor current came_from in
          let new_open_set =
            if is_in_open_set open_set neighbor then
              open_set  (* Node already in open set, would need to update priority *)
            else
              add_to_open_set open_set neighbor f_value
          in
          (new_open_set, closed_set, new_g_score, new_f_score, new_came_from)
        else
          (open_set, closed_set, g_score, f_score, came_from)
    in

    (* Main search function *)
    let rec search (open_set, closed_set, g_score, f_score, came_from) =
      let (current_opt, new_open_set) = remove_from_open_set open_set in
      match current_opt with
      | None -> None  (* No path found *)
      | Some current ->
          if current = goal then
            Some (reconstruct_path came_from goal)  (* Path found *)
          else
            let new_closed_set = NodeSet.add current closed_set in

            (* Process all neighbors *)
            let neighbors_list = neighbors current in
            let new_state = List.fold_left
              (process_neighbor current)
              (new_open_set, new_closed_set, g_score, f_score, came_from)
              neighbors_list
            in

            search new_state
    in

    search (initial_open_set, initial_closed_set, initial_g_score, initial_f_score, initial_came_from)
end

(* Example usage on a 2D grid *)
module GridAStar = struct
  open AStar

  (* Define grid dimensions *)
  let width = 10
  let height = 10

  (* Example grid - 0 is walkable, 1 is obstacle *)
  let grid = [|
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;0;1;1;0;0;0;0;0;0|];
    [|0;0;0;1;0;0;0;0;0;0|];
    [|0;0;0;1;0;0;0;0;0;0|];
    [|0;0;0;1;1;0;0;0;0;0|];
    [|0;0;0;0;1;0;0;0;0;0|];
    [|0;0;0;0;1;0;0;0;0;0|];
    [|0;0;0;0;1;0;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
    [|0;0;0;0;0;0;0;0;0;0|];
  |]

  (* Check if a position is valid *)
  let is_valid_position (x, y) =
    x >= 0 && x < width && y >= 0 && y < height && grid.(y).(x) = 0

  (* Get valid neighbors for a position *)
  let get_neighbors (x, y) =
    let possible_moves = [
      (x+1, y); (x-1, y); (x, y+1); (x, y-1);
      (x+1, y+1); (x+1, y-1); (x-1, y+1); (x-1, y-1)
    ] in
    List.filter is_valid_position possible_moves

  (* Cost function - Euclidean distance for diagonal movement *)
  let cost (x1, y1) (x2, y2) =
    let dx = float_of_int (x2 - x1) in
    let dy = float_of_int (y2 - y1) in
    sqrt (dx *. dx +. dy *. dy)

  (* Heuristic function - Manhattan distance *)
  let heuristic (x1, y1) (x2, y2) =
    let dx = abs (x2 - x1) in
    let dy = abs (y2 - y1) in
    float_of_int (dx + dy)

  (* Find path between two points *)
  let find_path start goal =
    AStar.astar start goal get_neighbors cost heuristic
end

(* Example of using the algorithm *)
let () =
  let start = (0, 0) in
  let goal = (9, 9) in
  match GridAStar.find_path start goal with
  | None ->
      Printf.printf "No path found from (%d,%d) to (%d,%d)\n"
        (fst start) (snd start) (fst goal) (snd goal)
  | Some path ->
      Printf.printf "Path found from (%d,%d) to (%d,%d):\n"
        (fst start) (snd start) (fst goal) (snd goal);
      List.iter (fun (x, y) -> Printf.printf "(%d,%d) " x y) path;
      Printf.printf "\n"
