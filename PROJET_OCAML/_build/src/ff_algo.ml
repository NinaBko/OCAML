open Graph

type path = id list

type res_graph = {
  gr : (int*int) graph;
  source : id;
  dest : id;} 

(*Init a residual graph with (0,flow) on edges *)
let init_residual_graph gr source dest = 
  {gr = Tools.gmap gr (fun x -> (0,x)); 
   source = source; 
   dest = dest}
(*(Tools.gmap gr (fun x -> (0,x)),0,source,dest)*)

let get_graph res = 
  Tools.gmap res.gr (fun (x,y) -> string_of_int x ^ "/" ^string_of_int y)

let rec find_in_list l e = 
  match l with
  |[] -> false
  |x::rest -> (x = e) || find_in_list rest e

let arc_non_valide (a,(b,c)) = b = c

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

let string_of_path path = String.concat " -> " (List.map (fun x -> string_of_int x) path)