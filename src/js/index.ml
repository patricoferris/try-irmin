open Brr 
open Brr_lwd
open Code_mirror
open Top

module Worker = Brr_webworkers.Worker
module Toprpc = Toplevel_api.Make (Rpc_lwt.GenClient ())

let dark_theme_ext = 
  let dark = Jv.get Jv.global "__CM__dark" in
  Extension.of_jv @@ Jv.get dark "oneDark"

let ml_like = Jv.get Jv.global "__CM__mllike" |> Language.of_jv

let set_classes el cl = 
  List.iter (fun v -> El.set_class v true el)  (List.map Jstr.v cl)

let set_inner_html el html =
  let jv = El.to_jv el in
  Jv.set jv "innerHTML" (Jv.of_string html)

let get_el_by_id s = Option.get @@ Document.find_el_by_id G.document (Jstr.v s)

let red el = 
  set_classes el [ "text-red-900" ]; el

let blue el = 
  set_classes el [ "text-blue-900" ]; el

let output_div, output_set =
  let open Let_syntax in 
  let s = Lwd.var ("", "", "") in
  let _c, stdout, _stderr = El.div [ El.txt' "Types" ], El.div [ El.txt' "Stdout" ], El.div [ El.txt' "Stderr" ] in
  (* set_classes c ["px-2"; "py-2"; "h-full"; "overflow-scroll"; "grid-cols-1"; "lg:col-span-3"; "border-2"; "border-light-green-500"]; *)
  (* set_classes stderr ["px-2"; "py-2"; "h-full"; "overflow-scroll"; "grid-cols-1"; "lg:col-span-3"; "border-2"; "border-light-green-500"]; *)
  set_classes stdout ["px-2"; "py-2"; "h-full"; "overflow-scroll"; "grid-cols-1"; "lg:col-span-6"; "border-2"; "border-light-green-500"];
  let div =
    let div = El.div [] in
    let+ _caml, stdout_out, stderr_out = Lwd.get s in
    (* El.append_children c [ El.(p [ txt' caml ]) ];
    El.append_children stderr [ El.(p [ txt' stderr_out ]) ]; *)
    El.append_children stdout El.[ div @@ El.[blue @@ p [ txt' stdout_out ]; red @@ p [ txt' stderr_out ]]];
    set_classes div [ "font-mono"; "h-full"; "text-sm"; "grid"; "grid-cols-1"; "items-start"; "lg:grid-cols-6" ];
    El.set_children div [ stdout; ];
    div
  in
  let set caml stdout stderr = 
    Lwd.set s (Option.value ~default:"" caml, Option.value ~default:"" stdout, Option.value ~default:"" stderr)
  in
  Elwd.div [ `R div ], set

let handle_output (o : Toplevel_api.exec_result) =
  output_set o.caml_ppf o.stdout o.stderr

open Lwt.Infix

let rpc_bind x f =
  x |> Rpc_lwt.T.get >>= function
  | Ok x ->
    f x
  | Error (Toplevel_api.InternalError s) ->
    Lwt.fail (Failure (Printf.sprintf "Rpc failure: %s" s))

let timeout_container worker () =
  let open Brr in
  Worker.terminate worker;
  match Document.find_el_by_id G.document @@ Jstr.v "toplevel-container" with
  | Some el ->
    El.(
      set_children
        el
        [ El.p
            [ El.txt' "Toplevel terminated after timeout on previous execution"
            ]
        ])
  | None ->
    ()

let rpc = 
  let worker =
    try Worker.create (Jstr.v "./worker.js") with
    | Jv.Error _ ->
      failwith "Failed to created worker"
  in
  let context = Rpc_brr.Worker_rpc.start worker 20 (timeout_container worker) in
  Rpc_brr.Worker_rpc.rpc context


let _state, view = 
  let ml = Stream.Language.define ml_like in
  Edit.init ~doc:(Jstr.v Example.v) ~exts:[| dark_theme_ext; ml; Editor.View.line_wrapping () |] ()

let (let*) = rpc_bind
let toolbar =
  let run = [ El.txt' "▶ Run" ] in
  let button = El.(button run) in
  let on_click _ =
    let run () =
      El.set_children button [];
      El.set_class (Jstr.v "loader") true button;
      let* o = Toprpc.exec rpc (Edit.get_doc view ^ ";;") in
      El.set_class (Jstr.v "loader") false button;
      El.set_children button run;
      handle_output o;
      Lwt.return ()
    in 
    Lwt.async run
  in
  Ev.(listen click on_click (El.as_target button));
  let open Brr_lwd.Let_syntax in
  let+ div = Elwd.div [ `P button ] in
  set_classes div [ "editor-toolbar"; "text-white"; "font-bold"; "px-4" ];
  div
      
let setup () =
  let setup () =
    let* o = Toprpc.setup rpc () in
    handle_output o;
    Lwt.return ()
  in
  setup () >>= fun _ ->
  Lwt.return ()
  
let () =
  let elements =
    let open Brr_lwd.Let_syntax in
    let* output = output_div in
    let+ tb = toolbar in
    [ tb; output ]
  in
  onload ~el:(get_el_by_id "markdown") elements;
  Lwt.async setup