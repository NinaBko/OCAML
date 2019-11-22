open Graph

type path = id list

type res_graph = {
  gr : (int*int) graph;
  source : id;
  dest : id;} 

(*Init a residual graph with (0,flow) on edges *)
val init_residual_graph: int graph -> id -> id -> res_graph

(*Find a path in the residual graph from source to destination node*)
val find_path: res_graph -> path

(*Find the max augmenting value*)
val find_max_aug: path -> res_graph -> int

val apply_aug: path -> res_graph -> int -> res_graph

val ff: int graph -> id -> id -> res_graph
(*
val get_max_flow: res_graph -> int

*)
val get_graph: res_graph -> string graph

val string_of_path: path-> string