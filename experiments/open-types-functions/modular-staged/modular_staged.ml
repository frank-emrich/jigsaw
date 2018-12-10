open Shared
open Modular_staged_types




let lift_let_term : (term Let_types.let_term -> term) code = .< fun lt  ->  LetTerm lt >.
let lift_core_typ : (typ Core_types.core_typ -> typ) code = .< fun ct ->  CoreTyp ct >.
let lift_core_term : ((term, typ) Core_types.core_term -> term)  code = .< fun ct -> CoreTerm ct >.
let lift_core_value : ((value, term) Core_types.core_value -> value) code = .< fun v  -> CoreValue v >.
let unlift_core_value  = .< fun v -> match v with
   | CoreValue v -> Some v
   | _ -> None >.
let unlift_core_typ  = .< fun t ->match t with
   | CoreTyp t -> Some t
   | _ -> None >.
let unlift_core_term  = .< fun t -> match t with
  | CoreTerm t -> Some t
  | _ -> None >.

let unlift_let_term = .< fun t -> match t with
  | LetTerm t -> Some t
  | _ -> None >.


let unlift_arith_term  = .< fun t -> match t with
  | ArithTerm t -> Some t
  | _ -> None >.

let unlift_arith_typ  = .< fun t -> match t with
  | ArithTyp t -> Some t
  | _ -> None >.


let unlift_arith_value = .< fun t -> match t with
  | ArithValue t -> Some t
  | _ -> None >.

let lift_arith_term : (term Arith_types.arith_term -> term) code  = .< fun t -> ArithTerm t >.
let lift_arith_value : (( Arith_types.arith_value) -> value) code  = .< fun v ->  ArithValue v >.
let lift_arith_typ : (Arith_types.arith_typ -> typ) code = .< fun t ->  ArithTyp t >.


let core_tc_deps : (typ tenv -> term -> typ) code -> (term, typ) Core.core_tc_deps = fun typecheck -> {
        cctd_lift_core_typ_to_typ =  lift_core_typ;
        cctd_unlift_typ_to_core_typ = unlift_core_typ;
        cctd_lift_core_term_to_term = lift_core_term;
        cctd_unlift_term_to_core_term = unlift_core_term;
        cctd_typecheck = typecheck
      }

let typecheck_code =
  .<
  let rec typecheck env term : typ = match term with
      | CoreTerm t  ->
        .~(Core.core_typecheck_code (core_tc_deps .< typecheck >. )) env t
      | LetTerm t ->
        .~(let deps : (term, typ) Let.let_tc_deps = {
          ltcd_lift_let_term_to_term = lift_let_term;
          ltcd_unlift_term_to_let_term = unlift_let_term;
          ltcd_typecheck = .< typecheck >.
        } in Let.let_typecheck_code deps ) env t
      | ArithTerm t ->
        .~(let deps : (term, typ) Arith.arith_tc_deps = {
          atcd_lift_core_typ_to_typ = lift_core_typ;
          atcd_lift_arith_typ_to_typ = lift_arith_typ;
          atcd_unlift_typ_to_arith_typ = unlift_arith_typ;
          atcd_lift_arith_term_to_term = lift_arith_term;
          atcd_unlift_term_to_arith_term = unlift_arith_term;
          atcd_typecheck = .< typecheck >.;
        } in Arith.arith_typecheck_code deps) env t in
      typecheck
  >.


let eval_code =
  .<
    let rec eval  = fun env  term  -> match term with
      | CoreTerm t ->
          .~(let deps : (term, typ, value) Core.core_eval_deps =
          {
            ced_lift_core_typ_to_typ = lift_core_typ;
            ced_unlift_typ_to_core_typ = unlift_core_typ;
            ced_lift_core_term_to_term = lift_core_term;
            ced_unlift_term_to_core_term = unlift_core_term;
            ced_lift_core_value_to_value = lift_core_value;
            ced_unlift_value_to_core_value = unlift_core_value;
            ced_eval = .< eval >.;
          } in Core.core_eval_code deps) env t
      | LetTerm t ->
          .~(let deps : (term, typ, value) Let.let_eval_deps = {
            led_lift_let_term_to_term = lift_let_term;
            led_unlift_term_to_let_term = unlift_let_term;
            led_eval = .< eval >. ;
          } in Let.let_eval_code deps) env t
        | ArithTerm t ->
          .~(let deps : (term, typ, value ) Arith.arith_eval_deps = {
            aed_lift_arith_value_to_value = lift_arith_value;
            aed_unlift_value_to_arith_value = unlift_arith_value;
            aed_lift_arith_term_to_term = lift_arith_term;
            aed_unlift_term_to_arith_term = unlift_arith_term;
            aed_eval = .< eval >. ;
            aed_lift_core_value_to_value = lift_core_value
          } in Arith.arith_eval_code deps) env t in
    (eval :  (value venv -> term -> value))
  >.

(* example program:
  let rec sum x =
    if x = 1 then
      1
    else
      let m = x-1 in
      let sm1 = sum m in
      let sm2 = sum m in (* provoke exponential number of calls *)
      1 + sm
  sum 500

*)


let recLamE (a,b,c,d,e) = Modular_staged_types.CoreTerm (Core_types.RecLamE (a,b,c,d,e))

let ifE (a,b,c) = Modular_staged_types.CoreTerm (Core_types.IfE (a,b,c))
let intEq (a,b) = Modular_staged_types.ArithTerm (Arith_types.IntEq (a,b))
let varE x = Modular_staged_types.CoreTerm (Core_types.VarE x)
let intE x = Modular_staged_types.ArithTerm (Arith_types.IntE x)
let plus (a,b) = Modular_staged_types.ArithTerm (Arith_types.PlusE (a,b))
let app (a,b) = Modular_staged_types.CoreTerm (Core_types.AppE (a,b))

let letE (a,b,c) = Modular_staged_types.LetTerm (Let_types.Let (a,b,c))

let intT = Modular_staged_types.ArithTyp Arith_types.IntT




let else_branch : term =
  letE (
    "m",
    plus (
      varE "x",
      intE (-1)),
    letE (
      "sm1",
      app (
        varE "sum",
        varE "m"),
      letE (
        "sm2",
         app (
           varE "sum",
           varE "m"),
        plus (
          intE 1,
          varE "sm1")
      )

    )
  )


let sum =
  recLamE ("sum", intT, "x", intT,
    ifE (
      intEq (
        varE "x",
        intE 0),
      intE 0,
      else_branch
    ))


let program =
  letE (
    "sum",
    sum,
    app (
      varE "sum",
      intE 23
    )
  )



let time msg f x =
  if Shared.debug then
    let t = Sys.time() in
    let fx = f x in
    Printf.eprintf "%s execution time: %fs\n" msg (Sys.time() -. t);
    flush stderr;
    fx
  else
    f x


let (use_cache, compilation_flags) =
  match Array.to_list Sys.argv with
    | _ :: "use-caching" :: cf -> true, String.concat " " cf
    | _ :: "no-caching" :: cf -> false, String.concat " " cf
    | _ -> failwith "Wrong args"




let stage () =
  (if use_cache then
    prerr_endline "using caching";
    Cached_runnative.Cache.attempt_load_from_disk "compiled_file_cache.blob");
  Cached_runnative.run compilation_flags use_cache typecheck_code,
  Cached_runnative.run compilation_flags use_cache eval_code



let typecheck, eval = time "staging" stage ()


let curry2 f (x,y) = f x y


let t = time "typechecking" (curry2 typecheck) ([], program)



(*let _ = print_endline (show_typ t)*)

let res = eval [] program

(*let _ = print_endline (show_value res)*)


let _ =
  if use_cache then
    Cached_runnative.Cache.save_if_changed "compiled_file_cache.blob"