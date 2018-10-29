module StringMap = Map.Make(String)


let string_map_of_seq (seq : (string * 'a ) list) : 'a StringMap.t =
  List.fold_left (fun map (k, v) ->
    StringMap.add k v map
  ) StringMap.empty seq

let seq_of_hashtbl map =
  Hashtbl.fold (fun k v result_list  -> (k,v) :: result_list ) map []

let hashtbl_of_seq seq_list =
  let tbl = Hashtbl.create (List.length seq_list) in
  List.iter (fun (k,v) -> Hashtbl.add tbl k v) seq_list;
  tbl