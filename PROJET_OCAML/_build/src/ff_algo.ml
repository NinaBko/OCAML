open Graph

type path = id list

type res_graph = {
  gr : (int*int) graph;
  source : id;
  dest : id;} 

(* Init a residual graph with (0,flow) on edges *)
let init_residual_graph gr source dest = 
  {gr = Tools.gmap gr (fun x -> (0,x)); 
   source = source; 
   dest = dest}
(*(Tools.gmap gr (fun x -> (0,x)),0,source,dest) *)

(* Map edges to string *)
let get_graph res = 
  Tools.gmap res.gr (fun (x,y) -> string_of_int x ^ "/" ^string_of_int y)

(* Returns true if the element e is in the list l *)
let rec find_in_list l e = 
  match l with
  |[] -> false
  |x::rest -> (x = e) || find_in_list rest e

(* Returns true if the edge is not valid *)
let arc_non_valide (a,(b,c)) = b = c

(* Returns a path from source to destination in the residual graph *)
let find_path graph = 
  (* Check if we have reached destination node *)
  let rec loop graph path liste_visite = if graph.source = graph.dest then path else
      (* Extract out arcs from actual node *)
      let arcs = out_arcs graph.gr graph.source in
      let rec find_arc arcs = match arcs with
        |[] -> if path = [] then [] else loop graph (List.tl path) liste_visite
        |(x,y)::rest -> if find_in_list  liste_visite x || arc_non_valide (x,y) then find_arc rest
          else loop {graph with source = x} (x::path) (x::liste_visite)
      in
      find_arc arcs
  in
  match loop graph [graph.source] [] with
  | [] -> failwith "Path not found"
  | p -> List.rev p

(* Makes the path to string *)
let string_of_path path = String.concat " -> " (List.map (fun x -> string_of_int x) path)




(*Create a path with edges from the path we found *)
(*Create a list of all edges labels used in the path*)
let create_edge_path path graph= 
  let rec loop path graph acu= 
    match path with 
    | _::[]|[]-> acu
    | x :: y :: rest -> loop (List.tl path) graph (find_arc graph x y :: acu)   
  in 
  loop path graph []

(*Find the max flow of a graph*)
let find_max_flow graph =
  let arc_list = out_arcs graph.gr graph.source in 
  let rec loop l acu =
    match l with
    |[]->acu
    |(_,(_,y))::rest->if y>acu then loop rest y else loop rest acu  
  in 
  loop arc_list 0

(* Find the max augmenting value *)
let find_max_aug path graph=
  let arc_list = create_edge_path path graph.gr in
  let rec loop path acu =
    match path with
    |[]->acu
    |Some(x,y)::rest-> if (y-x)<acu then loop rest y-x else loop rest acu
    |None::rest->failwith "Erreur label arc dans path"
  in
  loop arc_list (find_max_flow graph)
