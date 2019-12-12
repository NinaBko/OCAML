open Graph
open Tools

type path = id list

type res_graph = {
  gr : (int*int) graph;
  source : id;
  dest : id;} 

type res_graph_bis = {
  gr_bis : (int*int) graph;
  source_bis : id;
  dest_bis : id;} 


let init_graph gr source dest = 
  {gr = Tools.gmap gr (fun x -> (0,x)); 
   source = source; 
   dest = dest}

let create_res_arcs gr = 
  let gr_bis = clone_nodes gr in 
  let gr_arc_direct = e_fold gr (fun gr_acu id1 id2 (x,y) -> new_arc gr_acu id1 id2 (y-x,0)) gr_bis in 
  e_fold gr (fun gr id1 id2 (x,y) -> new_arc gr id2 id1 (x,1)) gr_arc_direct



let init_residual_graph gr source dest = 
  {gr_bis = create_res_arcs gr;
   source_bis = source; 
   dest_bis = dest}

(* Map edges to string *)
let get_graph res = 
  Tools.gmap res (fun (x,y) -> string_of_int x ^ "/" ^string_of_int y)

let get_graph_bis res_bis = 
  Tools.gmap res_bis.gr_bis (fun (x,y) -> string_of_int x)

(* Returns true if the element e is in the list l *)
let rec find_in_list l e = 
  match l with
  |[] -> false
  |x::rest -> (x = e) || find_in_list rest e

(* Returns true if the edge is not valid *)
let arc_non_valide (a,(b,c)) = 
  b=0

(* Returns a path from source to destination in the residual graph *)
let find_path graph = 
  (* Check if we have reached destination node *)
  let rec loop graph path liste_visite = if graph.source_bis = graph.dest_bis then path else
      (* Extract out arcs from actual node *)
      let arcs = out_arcs graph.gr_bis graph.source_bis in
      let rec find_arc arcs = match arcs with
        |[] -> []
        |(x,y)::rest -> if find_in_list liste_visite x || arc_non_valide (x,y) then find_arc rest
          else 
            let result = loop {graph with source_bis = x} (x::path) (x::liste_visite) in 
            match result with
            |[]-> find_arc rest
            |_-> result
      in
      find_arc arcs
  in
  match loop graph [graph.source_bis] [graph.source_bis] with
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
  let arc_list = out_arcs graph.gr_bis graph.source_bis in 
  let rec loop l acu =
    match l with
    |[] -> acu
    |(_,(y,_))::rest -> if y>acu then loop rest y else loop rest acu  
  in 
  loop arc_list 0

(* Find the max augmenting value *)
let find_max_aug path graph =
  let arc_list = create_edge_path path graph.gr_bis in
  let rec loop path acu =
    match path with
    |[] -> Printf.printf "\n" ; acu
    |Some(x,_)::rest -> if (x)<acu then loop rest x else loop rest acu
    |None::rest -> failwith "Erreur label arc dans path"
  in
  loop arc_list (find_max_flow graph)

let rec apply_aug path graph aug = 
  match path with
  |_::[]|[] -> graph
  |x::y::rest -> apply_aug (y::rest) {graph with gr_bis = Tools.add_arc_flow (Tools.add_arc_flow graph.gr_bis y x aug) x y (-aug)} aug


let ff graph source dest =
  let rec loop final_graph =
    let path = find_path final_graph in
    match path with
    |_::[]|[]->final_graph
    |_-> let str = string_of_path path in 
      let aug = find_max_aug path final_graph in
      Printf.printf "path %s        %d%!\n" str aug;
      loop (apply_aug path final_graph aug ) in 
  loop (init_residual_graph (init_graph graph source dest).gr source dest)


let concat_graph gr_bis =
  let gr_final = clone_nodes gr_bis in 
  e_fold gr_bis (fun gr_acu id1 id2 (x,y) -> 
      match y with
      |0->
        let opt_retour = find_arc gr_bis id2 id1 in 
        let val_retour =
          match opt_retour with
          |Some(z,_) -> z
          |None->failwith "Erreur label arc dans path"
        in 
        new_arc gr_acu id1 id2 (val_retour,val_retour+x)
      |_-> gr_acu) gr_final  
