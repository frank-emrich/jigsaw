type 'term let_term = 
    LetE of Core.Ir.var * 'term * 'term

let let_typecheck
    (typecheck :
      'typ Core.Ir.tenv -> 'term -> 'typ)
    (env : 'typ  Core.Ir.tenv) (term : 'term let_term)
    =
  match term with
   | LetE (x, m, n) ->
        let xt = typecheck env m in
        let env' = (x, xt) :: env in
        typecheck  env' n


let let_eval 
  (eval : 'value Core.Ir.venv -> 'term -> 'value )
  (env : 'value Core.Ir.venv) (term : 'term let_term) =
  match term with
 | LetE (x, m, n ) ->
  let xv = eval env m in
  let env' = (x, xv) :: env in
  eval env' n