type 'term let_term =
  | Let of Shared.var * 'term * 'term
    [@@deriving show]


type ('term, 'typ) let_tc_deps =
{
  ltcd_lift_let_term_to_term : 'term   let_term -> 'term;
  ltcd_unlift_term_to_let_term : 'term -> (('term)  let_term) option;
  ltcd_typecheck :  'typ Shared.tenv -> 'term -> 'typ
} [@@deriving show]

let let_typecheck
      (deps : ('term, 'typ) let_tc_deps)
      (env : 'typ Shared.tenv)
      (term : ('term)  let_term) : 'typ =
    match deps.ltcd_unlift_term_to_let_term (deps.ltcd_lift_let_term_to_term term) with
      | Some (Let (var, te1, te2)) ->
        let ty1 = deps.ltcd_typecheck env te1 in
        let env' = (var, ty1) :: env in
        deps.ltcd_typecheck env' te2
      | None -> failwith "Impossible"


type ('term, 'typ,  'value) let_eval_deps =
{
  led_lift_let_term_to_term : ('term)  let_term -> 'term;
  led_unlift_term_to_let_term : 'term -> (('term)  let_term) option;
  led_eval : 'value  Shared.venv -> 'term -> 'value
} [@@deriving show]

let let_eval
   (deps: ('term, 'typ, 'value) let_eval_deps)
   (env : 'value Shared.venv)
   (term : ('term) let_term) : 'value =
   match deps.led_unlift_term_to_let_term (deps.led_lift_let_term_to_term term) with
    | Some (Let (var, te1, te2)) ->
      let v1 = deps.led_eval env te1 in
      let env' = (var, v1) :: env in
      deps.led_eval env' te2
    | None -> failwith "Impossible"



