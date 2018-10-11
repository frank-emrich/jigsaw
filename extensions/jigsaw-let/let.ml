type ('ext_term, 'ext_typ) let_term_ext = 
    LetE of Core.Ir.var * ('ext_term, 'ext_typ) Core.Ir.core_term * ('ext_term, 'ext_typ) Core.Ir.core_term

let let_typecheck
    (check_ext_term :
      'ext_typ Core.Ir.tenv -> 'ext_term -> 'ext_typ Core.Types.core_type)
    (env : 'ext_typ Core.Ir.tenv) (term : ('ext_term, 'ext_typ) let_term_ext)
    =
  match term with
   | LetE (x, m, n) ->
        let xt = Core.Ir.core_typecheck check_ext_term env m in
        let env' = (x, xt) :: env in
        Core.Ir.core_typecheck check_ext_term env' n


let let_eval 
  (eval_ext_term : ('ext_value, 'ext_term, 'ext_type) Core.Ir.venv -> 'ext_term -> ('ext_value, 'ext_term, 'ext_type) Core.Ir.core_value)
  (env : ('ext_value, 'ext_term, 'ext_type) Core.Ir.venv) (term : ('ext_term, 'ext_type) let_term_ext) =
  match term with
 | LetE (x, m, n ) ->
  let xv = Core.Ir.core_eval eval_ext_term env m in
  let env' = (x, xv) :: env in
  Core.Ir.core_eval eval_ext_term env' n