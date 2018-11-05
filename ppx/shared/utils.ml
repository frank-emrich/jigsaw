module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

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

let rec split_list_after i l =
  match l with
    | x :: xs when i > 0 ->
      let (y, z) = split_list_after (i-1) xs in
      (x :: y, z)
    | _ -> ([], l)



(* If mapi and map2 had a child ... *)
let mapi2
    (f : int -> 'a -> 'b -> 'c)
    (l1 : 'a list)
    (l2 : 'b list) =
  let rec mapi2indexed f l1 l2 i =
    match l1, l2 with
      | x :: l1', y :: l2' ->
        (f i x y) :: mapi2indexed f l1' l2' (i+1)
      | [], [] -> []
      | _ -> failwith "List length mismatch in mapi2" in
  mapi2indexed f l1 l2 0

let rec take i = function
  | x :: xs when i > 0 -> x :: take (i-1) xs
  | _ -> []

let rec last = function
  | [] -> failwith "last on empty list"
  | [x] -> x
  | _ :: xs -> last xs


let keys_of_stringmap map =
  StringMap.fold
    (fun k _ keys ->
      k :: keys)
    map
    []