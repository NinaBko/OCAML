open Graph

type path = id list


type res_graph = {
  gr : (int*int) graph;
  source : id;
  dest : id;} 

(*Type de notre graphe d'écarts permettant d'identifier le sens des arcs à l'état initial avec un 0 ou un 1 sur le 2eme int*)
type res_graph_bis = {
  gr_bis : (int*int) graph;
  source_bis : id;
  dest_bis : id;} 

(*Init a residual graph with (0,flow) on edges *)
val init_graph: int graph -> id -> id -> res_graph

(*Creation du graph d'écarts *)
val init_residual_graph: (int*int) graph -> id -> id -> res_graph_bis

(*Find a path in the graph from source to destination node*)
val find_path: res_graph_bis -> path

(*Find the max augmenting value*)
val find_max_aug: path -> res_graph_bis -> int

(* Change la valeur de nos arcs en fonction de l'augmentation possible *)
val apply_aug: path -> res_graph_bis -> int -> res_graph_bis

(*Ford-Fulkerson*)
val ff: int graph -> id -> id -> res_graph_bis

(*toString graph fonctions*)
val get_graph: (int*int) Graph.graph-> string graph

val get_graph_bis: res_graph_bis -> string graph

val string_of_path: path-> string

(*Renvoie le graphe de flow à partir d'un graphe d'écart*)
val concat_graph: (int*int) Graph.graph -> (int*int) Graph.graph