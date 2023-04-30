type 'a t = (string, 'a ref) Hashtbl.t list ref

let new_environment : unit -> 'a t = fun () -> ref [ Hashtbl.create 10 ]
let add_layer (env : 'a t) = env := Hashtbl.create 10 :: !env
let remove_layer (env : 'a t) = env := List.tl !env

let add (env : 'a t) var typ =
  let h = List.hd !env in
  Hashtbl.replace h var (ref typ)

let add_ref (env : 'a t) var reference =
  let h = List.hd !env in
  Hashtbl.replace h var reference

let get_ref (env : 'a t) var =
  List.fold_left
    (fun acc h -> match acc with None -> Hashtbl.find_opt h var | _ -> acc)
    None !env

let get (env : 'a t) var = Option.map (fun a -> !a) (get_ref env var)

let modify (env : 'a t) var typ =
  match get_ref env var with Some r -> r := typ | None -> add env var typ

let is_def_in_current_layer (env : 'a t) var = Hashtbl.mem (List.hd !env) var
