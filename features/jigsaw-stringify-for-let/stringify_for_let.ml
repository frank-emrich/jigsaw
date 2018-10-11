let stringify_let_term ext_term_stringifier ext_type_stringifier let_term = 
    let sfy = Stringify.Stringify.stringify_term ext_term_stringifier ext_type_stringifier in
    match let_term with 
        | Let.LetE (v, n, m) -> "let " ^ v ^ " = " ^ sfy n ^ " in " ^ sfy m