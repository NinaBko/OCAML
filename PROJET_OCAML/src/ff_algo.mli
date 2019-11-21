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


val find_max_aug: (int*int) list -> res_graph -> int
(*
val apply_aug: res_graph -> path -> int -> res_graph

val ff: int graph -> res_graph

val get_max_flow: res_graph -> int

*)
val get_graph: res_graph -> string graph

val string_of_path: path-> string