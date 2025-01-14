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
  let vertex_attributes v = [`Label v]  (* Cela définit l'attribut label du sommet *)
  let vertex_name v = "\"" ^ String.escaped v ^ "\""  (* Échappe les noms *)
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let export_to_image prog output_file =
  (* Création du graphe *)
  let g = G.create () in

  (* Ajout de la racine : le programme lancé *)
  let root_node = "Program (Root)" in
  G.add_vertex g root_node;

  (* Ajout des variables globales comme fils de la racine *)
  List.iter (fun (var_name, var_type) ->
    let var_node = Printf.sprintf "Global: %s : %s " var_name (typ_to_string var_type) in
    G.add_vertex g var_node;
    G.add_edge g root_node var_node
  ) prog.globals;

  (* Ajout des classes et de leur structure *)
  List.iter (fun cls ->
    let class_node = Printf.sprintf "Class: %s " cls.class_name in
    G.add_vertex g class_node;
    G.add_edge g root_node class_node;

    (* Ajout des attributs comme fils de la classe *)
    List.iter (fun (attr_name, attr_type) ->
      let attr_node = Printf.sprintf "Attr: %s : %s " attr_name (typ_to_string attr_type) in
      G.add_vertex g attr_node;
      G.add_edge g class_node attr_node
    ) cls.attributes;

    (* Ajout des méthodes comme fils de la classe *)
    List.iter (fun meth ->
      (* Construire le label de la méthode avec son nom, paramètres et type de retour *)
      let params_types = 
        match meth.params with 
        |[] -> ["unit"]
        |_ -> List.map (fun (_, typ) -> typ_to_string typ) meth.params
      in
      let method_label = 
        Printf.sprintf " (%s -> %s)"
          (String.concat " -> " params_types)
          (typ_to_string meth.return)
      in

      (* Créer un seul nœud pour la méthode avec l'étiquette complète *)
      let method_node = Printf.sprintf "Meth: %s_%s " cls.class_name meth.method_name in

      (* Définir l'étiquette du sommet de la méthode directement lors de la création du sommet *)
      let full_method_node = method_node ^ ": " ^ method_label in
      G.add_vertex g full_method_node;  
      G.add_edge g class_node full_method_node 
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
;;
