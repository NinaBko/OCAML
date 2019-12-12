open Gfile
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (*Convert the string graph to an int graph*)
  let graph_int = Tools.gmap graph (fun x -> int_of_string x) in

  (*Apply the Ford-Fulkerson algorithm*)
  let final_graph = Ff_algo.concat_graph (Ff_algo.ff graph_int _source _sink).gr_bis in

  (*create the file of the final graph*)
  let () = export outfile (Ff_algo.get_graph final_graph) in
  ()

