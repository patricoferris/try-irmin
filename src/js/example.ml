let v = {|open Lwt.Syntax
module Store = Irmin_mem.KV (Irmin.Contents.String)

(* Database configuration *)
let config = Irmin_mem.config ()

(* Commit author *)
let author = "Example <example@example.com>"

(* Commit information *)
let info fmt () = Irmin.Info.v ~date:0L ~author fmt

let main () =
  (* Open the repo *)
  let* repo = Store.Repo.v config in

  (* Load the main branch *)
  let* t = Store.master repo in

  (* Set key "foo/bar" to "testing 123" *)
  let* () =
    Store.set_exn t ~info:(info "Updating foo/bar") [ "foo"; "bar" ]
      "testing 123"
  in

  (* Get key "foo/bar" and print it to stdout *)
  let+ x = Store.get t [ "foo"; "bar" ] in
  Printf.printf "foo/bar => '%s'\n" x

(* Run the program *)
let _ = main ()

|}