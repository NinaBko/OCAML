open Graph
(*returns a new graph having the same nodes than gr, but no arc*)
let clone_nodes gr = n_fold gr new_node empty_graph 

(*maps all arcs of gr by function f*)
let gmap gr f = e_fold gr ( fun gr_acu id1 id2 lbl -> new_arc gr_acu id1 id2 (f lbl)) (clone_nodes gr)

(*adds n to the value of the arc between id1 and id2. If the arc does not exist, it is created.*)
let add_arc gr id1 id2 n = 
  match find_arc gr id1 id2 with
  | Some x -> new_arc gr id1 id2 (x + n)
  | None -> new_arc gr id1 id2 (n)


(*adds n to the value of the arc between id1 and id2. If the arc does not exist, it is created.*)
let add_arc_flow gr id1 id2 n = 
  match find_arc gr id1 id2 with
  | Some (x,y) -> new_arc gr id1 id2 ((x + n),y)
  | None -> new_arc gr id1 id2 (0,n)


let add_arc_capacity gr id1 id2 n = 
  match find_arc gr id1 id2 with
  | Some (x,y) -> new_arc gr id1 id2 (x,(y+n))
  | None -> new_arc gr id1 id2 (0,n)