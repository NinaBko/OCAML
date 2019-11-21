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

  let graph_int = Tools.gmap graph (fun x -> int_of_string x) in

  (*let graph_test = Tools.gmap graph (fun x -> x^"a") in*)
  (*let graph_test = Tools.add_arc graph_int 0 1 100 in*)
  (*let graph_test = Ff_algo.init_residual_graph graph_int 0 5 in

    let graph_test_str = Ff_algo.get_graph graph_test in*)
  let res_graph = Ff_algo.init_residual_graph graph_int _source _sink in
  let path_test = Ff_algo.find_path res_graph in
  let str = Ff_algo.string_of_path path_test in
  let () = Printf.printf "path %s \n  max aug : %d%!\n" str (Ff_algo.find_max_aug path_test res_graph) in
  (* Rewrite the graph that has been read. *)
  (*let () = write_file outfile graph in*)

  let () = export outfile graph in
  ()

