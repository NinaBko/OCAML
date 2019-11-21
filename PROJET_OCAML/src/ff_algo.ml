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
let create_edge_path path = 
  let rec loop path graph = 
    match path with 
    | _ :: [] | [] -> graph
    | x :: y :: rest -> new_arc graph x y 

(* Find the maxi augmenting value 
   let find_max_aug path graph = *)
