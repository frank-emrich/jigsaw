let stringify_let_term strinigfy_term  let_term = 
    let sfy = strinigfy_term in
    match let_term with 
        | Let.LetE (v, n, m) -> "let " ^ v ^ " = " ^ sfy n ^ " in " ^ sfy m