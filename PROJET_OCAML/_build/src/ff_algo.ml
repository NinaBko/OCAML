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
let arc_non_valide (a,(b,c)) = b>=c

(* Returns a path from source to destination in the residual graph *)
let find_path graph = 
  (* Check if we have reached destination node *)
  let rec loop graph path liste_visite = if graph.source = graph.dest then path else
      (* Extract out arcs from actual node *)
      let arcs = out_arcs graph.gr graph.source in
      let rec find_arc arcs = match arcs with
        |[] -> []
        |(x,y)::rest -> if find_in_list  liste_visite x || arc_non_valide (x,y) then find_arc rest
          else 
            let result = loop {graph with source = x} (x::path) (x::liste_visite) in 
            match result with
            |[]-> find_arc rest
            |_-> result
      in
      find_arc arcs
  in
  match loop graph [graph.source] [graph.source] with
  | [] -> []
  | p -> List.rev p

(* Makes the path to string *)
let string_of_path path = String.concat " -> " (List.map (fun x -> string_of_int x) path)

(*Create a path with edges from the path we found *)
(*Create a list of all edges labels used in the path*)
let create_edge_path path graph= 
  let rec loop path graph acu= 
    match path with 
    | _::[]|[] -> acu
    | x :: y :: rest -> loop (List.tl path) graph (find_arc graph x y :: acu)   
  in 
  loop path graph []

(*Find the max flow of a graph*)
let find_max_flow graph =
  let arc_list = out_arcs graph.gr graph.source in 
  let rec loop l acu =
    match l with
    |[] -> acu
    |(_,(_,y))::rest -> if y>acu then loop rest y else loop rest acu  
  in 
  loop arc_list 0

(* Find the max augmenting value *)
let find_max_aug path graph =
  let arc_list = create_edge_path path graph.gr in
  let rec loop path acu =
    match path with
    |[] -> Printf.printf "\n" ; acu
    |Some(x,y)::rest ->Printf.printf "(%d,%d)/%d/%d " x y (y-x) acu;  if (y-x)<acu && x<y then loop rest (y-x) else loop rest acu
    |None::rest -> failwith "Erreur label arc dans path"
  in
  loop arc_list (find_max_flow graph)

let rec apply_aug path graph aug = 
  match path with
  |_::[]|[] -> graph
  |x::y::rest -> apply_aug (y::rest) {graph with  gr =Tools.add_arc_flow (Tools.add_arc_capacity graph.gr y x aug) x y aug} aug


let ff graph source dest =
  let rec loop final_graph =
    let path = find_path final_graph in
    match path with
    |_::[]|[]->final_graph
    |_-> let str = string_of_path path in 
      let aug = find_max_aug path final_graph in
      Printf.printf "path %s        %d%!\n" str aug;
      loop (apply_aug path final_graph aug ) in 
  loop (init_residual_graph graph source dest)