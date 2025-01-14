open Kawa
open Graph
open Printf

module G = Imperative.Digraph.ConcreteBidirectional(struct
  type t = string
  let compare = String.compare
  let hash = Hashtbl.hash
  let equal = (=)
end)

module Dot = Graph.Graphviz.Dot(struct
  include G
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [`Label v]
  let vertex_name v = String.escaped v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let export_to_image prog output_file =
  (* Création du graphe *)
  let g = G.create () in

  (* Ajout de la racine : le programme lancé *)
  let root_node = "Program" in
  G.add_vertex g root_node;

  (* Ajout des variables globales comme fils de la racine *)
  List.iter (fun (var_name, _) ->
    let var_node = "Global_" ^ var_name in
    G.add_vertex g var_node;
    G.add_edge g root_node var_node
  ) prog.globals;

  (* Ajout des classes et de leur structure *)
  List.iter (fun cls ->
    let class_node = "Class_" ^ cls.class_name in
    G.add_vertex g class_node;
    G.add_edge g root_node class_node;

    (* Ajout des attributs comme fils de la classe *)
    List.iter (fun (attr_name, _) ->
      let attr_node = cls.class_name ^ "_Attr_" ^ attr_name in
      G.add_vertex g attr_node;
      G.add_edge g class_node attr_node
    ) cls.attributes;

    (* Ajout des méthodes comme fils de la classe *)
    List.iter (fun meth ->
      let method_node = cls.class_name ^ "_Meth_" ^ meth.method_name in
      G.add_vertex g method_node;
      G.add_edge g class_node method_node
    ) cls.methods
  ) prog.classes;

  (* Exportation dans un fichier temporaire au format DOT *)
  let dot_file = "graph.dot" in
  let oc = open_out dot_file in
  Dot.output_graph oc g;
  close_out oc;

  (* Conversion du fichier DOT en une image PNG *)
  let cmd = sprintf "dot -Tpng %s -o %s" dot_file output_file in
  let exit_code = Sys.command cmd in
  if exit_code <> 0 then
    Printf.eprintf "Erreur lors de la génération de l'image\n"
  else
    Printf.printf "Image générée avec succès : %s\n" output_file
