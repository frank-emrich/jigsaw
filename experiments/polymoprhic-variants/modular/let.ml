
type 'term let_term =
  [ `Let of Core.var * 'term * 'term ]
    




let let_typecheck
      typecheck
      (env : 'typ Core.tenv)
      (term : ('term)  let_term) : 'typ =
    match term with
      | (`Let (var, te1, te2)) ->
        let ty1 = typecheck env te1 in
        let env' = (var, ty1) :: env in
        typecheck env' te2


let let_eval
   eval
   (env : 'value Core.venv)
   (term : ('term) let_term) : 'value =
   match term with
    | (`Let (var, te1, te2)) ->
      let v1 = eval env te1 in
      let env' = (var, v1) :: env in
      eval env' te2