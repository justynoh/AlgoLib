module MkSearch : GRAPH -> GRAPHSEARCH = functor (G : GRAPH) ->
  struct

    module IntMap = Map.Make(Int)
    module IntSet = Set.Make(Int)

    include G

    let dfsmain ~init ~onvisit ~compare ~ontraversal ~onrevisit ~onfinish g =
      let visited = ref IntSet.empty in
      let time = ref 0 in
      let parent = ref IntMap.empty in
      let start = ref IntMap.empty in
      let finish = ref IntMap.empty in
      (* dfs from a particular vertex *)
      let rec dfsvisit acc u =
        (* not visited yet *)
        let acc = onvisit acc u in
        (* update info for this vertex *)
        visited := IntSet.add !visited u;
        start := IntMap.add_exn !start ~key:u ~data:!time; incr time;
        (* get all the neighbors of u, sort so we go in order *)
        let vs = neighborhood g u |> List.sort ~compare:(compare u) in
        let acc = List.fold vs
          ~f:(fun acc (v,w) -> if IntSet.mem !visited u then onrevisit acc u v w else
                               (parent := IntMap.add_exn !parent ~key:v ~data:u;
                                dfsvisit (ontraversal acc u v w) v))
          ~init:acc in
        (* update info for this vertex *)
        finish := IntMap.add_exn !finish ~key:u ~data:!time; incr time;
        onfinish acc u
      in
      (visited, parent, start, finish, dfsvisit)

    let dfs ~init ~onvisit ~compare ~ontraversal ~onrevisit ~onfinish g =
      let (visited, parent, start, finish, dfsvisit) =
        dfsmain init onvisit compare ontraversal onrevisit onfinish g in
      (* take care of multiple connected components *)
      let acc = List.init ~f:(fun i -> i) (size g)
             |> List.fold ~f:(fun acc u -> if IntSet.mem !visited u then acc else dfsvisit acc u) ~init:init in
      let pmap = !parent in
      let smap = !start in
      let fmap = !finish in
      (acc,
       (fun u -> if isvertex g u then IntMap.find pmap u
                                 else raise VertexOutOfBounds),
       (fun u -> if isvertex g u then (IntMap.find_exn smap u, IntMap.find_exn fmap u)
                                 else raise VertexOutOfBounds))

    let dfs1 ~init ~onvisit ~compare ~ontraversal ~onrevisit ~onfinish g s =
      let (_, parent, start, finish, dfsvisit) =
        dfsmain init onvisit compare ontraversal onrevisit onfinish g in
      let acc = dfsvisit init s in
      let pmap = !parent in
      let smap = !start in
      let fmap = !finish in
      (acc,
       (fun u -> if isvertex g u then IntMap.find pmap u
                                 else raise VertexOutOfBounds),
       (fun u -> if isvertex g u then match (IntMap.find smap u, IntMap.find fmap u) with
                                        (Some a, Some b) -> Some (a,b)
                                      | (None, None) -> None
                                      | _ -> raise (Failure "dfs1: only one of start/finish filled.")
                                 else raise VertexOutOfBounds))


    let bfsmain ~init ~onvisit ~compare ~ontraversal ~onrevisit ~onfinish g =
      let visited = ref IntSet.empty in
      let parent = ref IntMap.empty in
      let q = Queue.create () in
      (* bfs by one step *)
      let rec bfsvisit acc =
        match Queue.dequeue q with None -> acc | Some u ->
        (* not visited yet *)
        let acc = onvisit acc u in
        (* update info for this vertex *)
        visited := IntSet.add !visited u;
        (* get all the neighbors of u, sort so we go in order *)
        let vs = neighborhood g u |> List.sort ~compare:(compare u) in
        let acc = List.fold vs
          ~f:(fun acc (v,w) -> if IntSet.mem !visited v then onrevisit acc u v w else
                               (parent := IntMap.add_exn !parent ~key:v ~data:u;
                                Queue.enqueue q v;
                                ontraversal acc u v w))
          ~init:acc in
        onfinish (bfsvisit acc) u
      in
      (visited, parent, q, bfsvisit)

    let bfs ~init ~onvisit ~compare ~ontraversal ~onrevisit ~onfinish g =
      let (visited, parent, q, bfsvisit) =
        bfsmain init onvisit compare ontraversal onrevisit onfinish g in
      (* take care of multiple connected components *)
      let acc = List.init ~f:(fun i -> i) (size g)
             |> List.fold ~f:(fun acc u -> if IntSet.mem !visited u then acc else (Queue.enqueue q u; bfsvisit acc)) ~init:init in
      let pmap = !parent in
      (acc,
       (fun u -> if isvertex g u then IntMap.find pmap u else raise VertexOutOfBounds))

    let bfs1 ~init ~onvisit ~compare ~ontraversal ~onrevisit ~onfinish g s =
      let (_, parent, q, bfsvisit) =
        bfsmain init onvisit compare ontraversal onrevisit onfinish g in
      let acc = (Queue.enqueue q s; bfsvisit init) in
      let pmap = !parent in
      (acc,
       (fun u -> if isvertex g u then IntMap.find pmap u else raise VertexOutOfBounds))

  end
