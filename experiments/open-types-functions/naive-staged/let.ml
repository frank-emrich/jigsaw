open Let_types

type ('term, 'typ) let_tc_deps =
{
  ltcd_lift_let_term_to_term : ('term   let_term -> 'term) code;
  ltcd_unlift_term_to_let_term : ('term -> (('term)  let_term) option) code;
  ltcd_typecheck :  ('typ Shared.tenv -> 'term -> 'typ) code
}

let let_typecheck_code (deps : ('term, 'typ) let_tc_deps) : ('typ Shared.tenv -> ('term)  let_term -> 'typ)  code =
  .<
    fun
      (env)
      (term) ->
    match .~(deps.ltcd_unlift_term_to_let_term) (.~(deps.ltcd_lift_let_term_to_term) term) with
      | Some (Let (var, te1, te2)) ->
        let ty1 = .~(deps.ltcd_typecheck) env te1 in
        let env' = (var, ty1) :: env in
        .~(deps.ltcd_typecheck) env' te2
      | None -> failwith "Impossible"
  >.

type ('term, 'typ,  'value) let_eval_deps =
{
  led_lift_let_term_to_term : (('term)  let_term -> 'term) code;
  led_unlift_term_to_let_term : ('term -> (('term)  let_term) option) code;
  led_eval : ('value  Shared.venv -> 'term -> 'value) code
}

let let_eval_code (deps: ('term, 'typ, 'value) let_eval_deps) : ( 'value Shared.venv -> ('term) let_term -> 'value ) code =
  .<
    fun
      (env )
      (term ) ->
   match .~(deps.led_unlift_term_to_let_term) (.~(deps.led_lift_let_term_to_term) term) with
    | Some (Let (var, te1, te2)) ->
      let v1 = .~(deps.led_eval) env te1 in
      let env' = (var, v1) :: env in
      .~(deps.led_eval) env' te2
    | None -> failwith "Impossible"
  >.


