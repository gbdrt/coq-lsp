open Fleche

(* Put these in an utility function for plugins *)
let of_execution ~io ~what (v : (_, _) Coq.Protect.E.t) =
  match v with
  | { r; feedback = _ } -> (
    match r with
    | Coq.Protect.R.Completed (Ok goals) -> goals
    | Coq.Protect.R.Completed (Error (Anomaly err))
    | Coq.Protect.R.Completed (Error (User err)) ->
      let message =
        Format.asprintf "error when retrieving %s: %a" what Pp.pp_with (snd err)
      in
      Io.Report.message ~io ~lvl:Io.Level.error ~message;
      None
    | Coq.Protect.R.Interrupted -> None)

let extract_raw ~(contents : Contents.t) ~(range : Lang.Range.t) =
  let start = range.start.offset in
  let length = range.end_.offset - start in
  (* We need to be careful here as Doc.t always adds a last empty node on EOF,
     but somehow the offset of this node seems suspicious, it seems like the Coq
     parser increases the offset by one, we need to investigate. *)
  let length =
    if String.length contents.raw < start + length then
      String.length contents.raw - start
    else length
  in
  String.sub contents.raw start length

(* We output a record for each sentence in the document, linear order. Note that
   for unparseable nodes, we don't have an AST. *)
module AstGoals = struct
  (* Just to bring the serializers in scope *)
  module Lang = Lsp.JLang
  module Coq = Lsp.JCoq

  type 'a hyp =
    { names : string list
    ; ty : 'a
    }

  let hyp_to_yojson pp h =
    let name = String.concat "," h.names in
    match pp h.ty with
    | `String ty -> `String (name ^ ": " ^ ty)
    | _ -> assert false

  let from_hyps =
    List.map (fun h -> { names = h.Coq.Goals.names; ty = h.Coq.Goals.ty })

  type 'a goal =
    { hyps : 'a hyp list
    ; goal : 'a
    }
  [@@deriving to_yojson]

  let from_reified_goal g =
    { goal = g.Coq.Goals.ty; hyps = from_hyps g.Coq.Goals.hyps }

  (* type 'a goals = { goals : 'a goal list ; stack : ('a goal list * 'a goal
     list) list } [@@deriving to_yojson] (* { stack : ('a list * 'a list) list ;
     goals : 'a list } [@@deriving to_yojson] *) *)

  type 'a t =
    { state : 'a goal option
    ; code : string
    ; goals : 'a goal list
    }
  [@@deriving to_yojson]

  let get_goals ~io (node : Doc.Node.t) =
    let st = node.state in
    match of_execution ~io ~what:"goals" (Info.Goals.goals ~st) with
    | None -> []
    | Some goals -> List.map from_reified_goal goals.goals

  let of_node ~io ~(contents : Contents.t) (pre_node : Doc.Node.t option)
      (node : Doc.Node.t) =
    let range = node.range in
    let code = extract_raw ~contents ~range in
    let state =
      match pre_node with
      | None -> None
      | Some pre_node -> (
        match get_goals ~io pre_node with
        | [] -> None
        | g :: _ -> Some g)
    in
    let goals = get_goals ~io node in
    (* let goals = match of_execution ~io ~what:"goals" (Info.Goals.goals ~st)
       with | None -> { stack = []; goals = [] } | Some goals -> let stack =
       List.map (fun (x, y) -> (List.map from_reified_goal x, List.map
       from_reified_goal y)) goals.stack in let goals = List.map
       from_reified_goal goals.goals in { stack; goals } in *)
    { code; state; goals }
end

let pp_json pp fmt (astgoal : _ AstGoals.t) =
  match astgoal.state with
  | None -> ()
  | Some state ->
    if astgoal.goals <> [ state ] then
      let g_json = AstGoals.to_yojson pp astgoal in
      Yojson.Safe.pretty_print fmt g_json
    else ()

(* For now we have not added sexp serialization, but we can easily do so *)
(* let pp_sexp fmt (astgoal : AstGoals.t) = *)
(*   let g_sexp = AstGoals.sexp_of astgoal in *)
(*   Sexplib.Sexp.pp_hum fmt sast *)

let pw pp fmt v = Format.fprintf fmt "@[%a@]@," pp v

let pp_ast_goals ~io ~contents pp fmt pre_node node =
  let res = AstGoals.of_node ~io ~contents pre_node node in
  pw pp fmt res

let pp_trace ~io ~contents pp fmt nodes =
  let rec trace pre_node nodes =
    match nodes with
    | [] -> ()
    | n :: nodes ->
      pp_ast_goals ~io ~contents pp fmt pre_node n;
      trace (Some n) nodes
  in
  trace None nodes

let dump_goals ~io ~out_file ~(doc : Doc.t) pp =
  let out = Stdlib.open_out out_file in
  let fmt = Format.formatter_of_out_channel out in
  let contents = doc.contents in
  (* List.iter (pp_ast_goals ~io ~contents pp fmt) doc.nodes; *)
  pp_trace ~io ~contents pp fmt doc.nodes;
  Stdlib.close_out out

let dump_ast ~io ~(doc : Doc.t) =
  let uri = doc.uri in
  let uri_str = Lang.LUri.File.to_string_uri uri in
  let message = Format.asprintf "[coq-dojo plugin] Tracing %s ..." uri_str in
  let lvl = Io.Level.info in
  Io.Report.message ~io ~lvl ~message;
  let out_file_j = Lang.LUri.File.to_string_file uri ^ ".trace.json" in
  let pp d = `String (Pp.string_of_ppcmds d) in
  (* Uncomment to output Pp-formatted goals *)
  (* let pp d = Lsp.JCoq.Pp.to_yojson d in *)
  let () = dump_goals ~io ~out_file:out_file_j ~doc (pp_json pp) in
  (* let out_file_s = Lang.LUri.File.to_string_file uri ^ ".sexp.goaldump" in *)
  (* let () = dump_goals ~out_file:out_file_s ~doc pp_sexp in *)
  let message =
    Format.asprintf "[coq-dojo plugin] Tracing %s completed!" uri_str
  in
  Io.Report.message ~io ~lvl ~message;
  ()

let main () = Theory.Register.add dump_ast
let () = main ()
