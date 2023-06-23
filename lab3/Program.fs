#nowarn "40"
open System
open System.IO
open System.Collections.Generic

type Token =
    // static tokens
    | OPARENT
    | CPARENT
    | OCURL
    | CCURL
    | SEMICOLON
    | COMMA
    | DOT
    | ADD
    | SUB
    | MULT
    | DIV
    | EQUAL
    | GT
    | LT
    | OP of string
    // dynamic tokens
    | ID of string
    | STRING of string
    | NUMBER of float

let toString = (function x -> (x |> List.toArray |> String))

let tokenize source =
    let literal_tokens = Map [
        ('(', Token.OPARENT);
        (')', Token.CPARENT);
        ('{', Token.OCURL);
        ('}', Token.CCURL);
        (';', Token.SEMICOLON);
        (',', Token.COMMA);
        ('.', Token.DOT);
    ]

    let arithmetic_tokes = Map [
        ('+', Token.OP("+"));
        ('-', Token.OP("-"));
        ('*', Token.OP("*"));
        ('/', Token.OP("/"));
        
        ('=', Token.OP("="));
        ('>', Token.OP(">"));
        ('<', Token.OP("<"));
        ('|', Token.OP("|"));
        ('&', Token.OP("&"));
    ]

    let rec read_string_end acc = function
    | '\\'::'"'::t -> (toString (List.rev acc)), t
    | '"'::t -> (toString (List.rev acc)), t
    | h::t -> read_string_end (h::acc) t
    | [] -> failwith "read_string_end@ EOF before closing \" found"

    let rec read_comment = function
    | '@'::t -> t
    | _::t -> read_comment t
    | [] -> failwith "read_comment@ EOF before closing comment"

    let rec read_linecomment = function
    | '\n'::t -> t
    | _::t -> read_linecomment t
    | [] -> []

    let rec read_id acc = function
    | h::t when Char.IsWhiteSpace(h) -> (toString (List.rev acc)), t
    | h::t when Char.IsLetter(h) || Char.IsDigit(h) || h = '_' -> read_id (h::acc) t
    | h::t when h = '(' || h = ')' || h = '{' || h = '}' -> (toString (List.rev acc)), (h::t)
    | [] -> (toString (List.rev acc)), [] // isn't it an error though?
    | h::_ -> failwith ("read_id@ Unexpected symbol met: " + (string h))

    let rec read_number acc = function
    | h::t when Char.IsWhiteSpace(h) -> (toString (List.rev acc)), t
    | h::t when Char.IsDigit(h) -> read_number (h::acc) t
    | '.'::t -> read_number ('.'::acc) t
    | h::t when h = '(' || h = ')' -> (toString (List.rev acc)), (h::t)
    | [] -> (toString (List.rev acc)), [] // isn't it an error though?
    | h::_ -> failwith ("read_number@ Unexpected symbol met while reading digit: " + (string h))

    let rec tokenize_impl acc = function
    | h::t when Char.IsWhiteSpace(h) -> tokenize_impl acc t // chopping whitespaces
    | h::t when literal_tokens |> Map.containsKey h -> tokenize_impl ((literal_tokens |> Map.find h)::acc) t
    | '"'::t | '\\'::'"'::t -> 
        let read_string, remaining_source = read_string_end [] t
        tokenize_impl (Token.STRING( read_string)::acc) remaining_source
    | '@'::t -> 
        let remaining_source = read_comment t // comments of type @...@
        tokenize_impl acc remaining_source
    | '#'::t -> 
        let remaining_source = read_linecomment t // line comments - skip to \n
        tokenize_impl acc remaining_source

    | h::t when Char.IsLetter(h) ->
        let read_id, remaining_source = read_id [] (h::t)
        tokenize_impl (Token.ID(read_id)::acc) remaining_source

    | h::t when Char.IsDigit(h) ->
        let read_number, remaining_source = read_number [] (h::t)
        try 
            let parsed_number = System.Double.Parse(read_number, System.Globalization.CultureInfo.InvariantCulture)
            tokenize_impl (Token.NUMBER(parsed_number)::acc) remaining_source
        with
            _ -> failwith ("tokenize_impl@ Unrecognizable number met: " + read_number)
    | '-'::h::t when Char.IsDigit(h) ->
        let read_number, remaining_source = read_number [] (h::t)
        try 
            let parsed_number = System.Double.Parse("-" + read_number, System.Globalization.CultureInfo.InvariantCulture)
            tokenize_impl (Token.NUMBER(parsed_number)::acc) remaining_source
        with
            _ -> failwith ("tokenize_impl@ Unrecognizable number met: " + read_number)
    | h::' '::t when (arithmetic_tokes |> Map.tryFind h).IsSome ->
         tokenize_impl ((arithmetic_tokes |> Map.find h)::acc) t

    | h::_ -> failwith ("tokenize_impl@ Unsupported symbol met: " + (string h))
    | [] -> List.rev acc

    tokenize_impl [] source

type Expr =
    | NUMBER of double
    | STRING of string
    | BOOL of bool
    | ID of string
    | COND of Expr * Expr * Expr               // DUMMYLIST * DUMMYLIST * DUMMYLIST
    | LET of string * Expr                     // string * DUMMYLIST
    | SET of string * Expr                     // stirng * DUMMYLIST
    | FUNC of string * Expr * Expr * env * int // string * DUMMYARGLIST * DUMMYLIST * env * arity
    | CALL of string * Expr * int              // string * DUMMYLIST * arity
    | OP of string * Expr list
    | PRINT of Expr

    | DUMMYARGLIST of Expr list
    | DUMMYLIST of Expr list
    | DUMMYOP of string
    | DUMMY of string
and env = Map<string, Expr>

let parse token_list = 
    let rec parse_ids acc = function
        | Token.ID(id)::t -> parse_ids (id::acc) t
        | Token.CCURL::t -> List.rev acc, t
        | _ -> failwith "parse_function_parameters@ not found expected id"

    let keywords = ["let"; "set"; "makefun"; "if"; "then"; "else"; "print"]

    let rec parse_impl acc = function
        | [] -> List.rev acc, []

        | Token.ID(expr)::t when (List.tryFind (fun x -> x = expr) keywords).IsSome -> parse_impl (Expr.DUMMY(expr)::acc) t

        | Token.NUMBER(n)::t -> parse_impl (Expr.DUMMYLIST([Expr.NUMBER(n)])::acc) t
        | Token.ID("true")::t -> parse_impl (Expr.DUMMYLIST([Expr.BOOL(true)])::acc) t
        | Token.ID("false")::t -> parse_impl (Expr.DUMMYLIST([Expr.BOOL(false)])::acc) t
        | Token.ID(id)::t -> parse_impl (Expr.DUMMYLIST([Expr.ID(id)])::acc) t
        | Token.STRING(s)::t -> parse_impl (Expr.DUMMYLIST([Expr.STRING(s)])::acc) t

        | Token.CCURL::t -> List.rev acc, t
        | Token.OCURL::t ->
            let read_args, remaining_part = parse_impl [] t
            if List.forall (fun x -> match x with | Expr.DUMMYLIST([Expr.ID(_)]) -> true | _ -> false) read_args 
            then parse_impl (Expr.DUMMYARGLIST(read_args)::acc) remaining_part
            else failwith ("parse_impl@ non ids inside function args: " + (sprintf "%A" read_args))

        | Token.CPARENT::t -> List.rev acc, t
        | Token.OPARENT::t ->
            let read_exprs, remaining_part = parse_impl [] t
            match read_exprs with
            | Expr.DUMMYOP(op)::t -> parse_impl (Expr.DUMMYLIST([Expr.OP(op, t)])::acc) remaining_part
            | Expr.DUMMY("let")::Expr.DUMMYLIST([Expr.ID(id)])::Expr.DUMMYLIST(list)::[] -> parse_impl (Expr.DUMMYLIST([Expr.LET(id, Expr.DUMMYLIST(list))])::acc) remaining_part
            | Expr.DUMMY("set")::Expr.DUMMYLIST([Expr.ID(id)])::Expr.DUMMYLIST(list)::[] -> parse_impl (Expr.DUMMYLIST([Expr.SET(id, Expr.DUMMYLIST(list))])::acc) remaining_part

            // function declaration
            | Expr.DUMMY("makefun")::Expr.DUMMYLIST([Expr.ID(id)])::(Expr.DUMMYARGLIST(args_list) as args)::(Expr.DUMMYLIST(_) as body)::[] -> 
                parse_impl (DUMMYLIST([Expr.FUNC(id, args, body, Map<string, Expr>[], List.length args_list)])::acc) remaining_part

            // function call
            | Expr.DUMMYLIST([Expr.ID(id)])::t ->
                if List.forall (fun x -> match x with | Expr.DUMMYLIST(_) -> true | _ -> false) t
                then parse_impl (DUMMYLIST([Expr.CALL(id, Expr.DUMMYLIST(t), List.length t)])::acc) remaining_part
                else failwith ("parse_impl@ wrong function call syntax! Used as parameters for function call: " + (sprintf "%A" t)) 

            | Expr.DUMMYLIST(list)::t -> 
                let rec gather_dummy_lists acc = function
                | (Expr.DUMMYLIST(_) as list)::t' -> gather_dummy_lists (list::acc) t'
                | [] -> List.rev acc, []
                | waste -> 
                    printfn "unmatched expr: %A" waste
                    failwith "gather_dummy_lists@ misformat expression"

                let lists, _ = gather_dummy_lists [] (DUMMYLIST(list)::t)
                parse_impl (Expr.DUMMYLIST(lists)::acc) remaining_part
            | Expr.DUMMY("if")::(Expr.DUMMYLIST(_) as cond)
                ::Expr.DUMMY("then")::(Expr.DUMMYLIST(_) as expr1)
                ::Expr.DUMMY("else")::(Expr.DUMMYLIST(_) as expr2)::[] -> 
                    parse_impl (Expr.DUMMYLIST([Expr.COND(cond, expr1, expr2)])::acc) remaining_part
            // replace else with empty dummylist
            | Expr.DUMMY("if")::(Expr.DUMMYLIST(_) as cond)
                ::Expr.DUMMY("then")::(Expr.DUMMYLIST(_) as expr1)::[] ->
                    parse_impl (Expr.DUMMYLIST([Expr.COND(cond, expr1, DUMMY(""))])::acc) remaining_part

            | Expr.DUMMY("print")::(Expr.DUMMYLIST(_) as body)::[] -> parse_impl (Expr.DUMMYLIST([Expr.PRINT(body)])::acc) remaining_part

            | (Expr.NUMBER(_) as num_expr)::[] -> parse_impl (DUMMYLIST([num_expr])::acc) remaining_part
            | (Expr.STRING(_) as str_expr)::[] -> parse_impl (DUMMYLIST([str_expr])::acc) remaining_part
            | waste -> failwith ("parse_impl@ wrong parenthesis structure: " + (sprintf "%A" waste))

        | Token.OP(op)::t -> parse_impl (Expr.DUMMYOP(op)::acc) t
        | waste -> 
            printfn "unexpected token: %A" waste
            failwith "parse_impl@ unexpected token"

    let parsed_expr, remaining_part = parse_impl [] token_list
    if remaining_part <> [] then 
        printfn "unparsed part: %A" remaining_part
        failwith "parse_impl@ misformat expression"
    Expr.DUMMYLIST(parsed_expr)

let eval env ast = 
    let lookup name env = env |> Map.find name

    let numeric_operators = Map [
        ("+", ((function x -> x), (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.NUMBER(x + y))));
        ("-", ((function Expr.NUMBER(x) -> Expr.NUMBER(-x)), (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.NUMBER(x - y))));
        ("*", ((function x -> x), (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.NUMBER(x * y))));
        ("/", ((function Expr.NUMBER(x) -> Expr.NUMBER(1. / x)), (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.NUMBER(x / y))));
    ]
    let bool_operators = Map [
        ("&", ((function x -> x), (function (Expr.BOOL(x), Expr.BOOL(y)) -> Expr.BOOL(x && y))));
        ("|", ((function x -> x), (function (Expr.BOOL(x), Expr.BOOL(y)) -> Expr.BOOL(x || y))));
    ]
    let binary_operators = Map [
        (">", (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.BOOL(x > y)));
        ("<", (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.BOOL(x < y)));
    ]

    let rec eval_args_bool eval_fn env acc = fun x ->
        match x with
        | h::t -> 
            let evaluated, new_env = eval_fn env h
            match evaluated with
            | Expr.BOOL(_) -> 
                eval_args_bool eval_fn new_env (evaluated::acc) t
            | Expr.DUMMYLIST([Expr.BOOL(_) as boolean]) ->
                eval_args_bool eval_fn new_env (boolean::acc) t
            | Expr.NUMBER(n) -> 
                eval_args_bool eval_fn new_env (Expr.BOOL(Convert.ToBoolean n)::acc) t
            | Expr.DUMMYLIST([Expr.NUMBER(n)]) ->
                eval_args_bool eval_fn new_env (Expr.BOOL(Convert.ToBoolean n)::acc) t
            | waste -> failwith ("check_bool@ unevaluatable bool expression: " + (sprintf "%A" waste))
        | [] -> List.rev acc, env

    let rec eval_args_num eval_fn env acc = fun x ->
        match x with
        | h::t -> 
            let evaluated, new_env = eval_fn env h
            match evaluated with
            | Expr.NUMBER(_) -> 
                eval_args_num eval_fn new_env (evaluated::acc) t
            | Expr.DUMMYLIST([Expr.NUMBER(_) as number]) ->
                eval_args_num eval_fn new_env (number::acc) t
            | waste -> failwith ("check_number@ unevaluatable numeric expression: " + (sprintf "%A" waste))
        | [] -> List.rev acc, env

    let rec eval_impl env = function
        | Expr.NUMBER(_) as number -> number, env
        | Expr.STRING(_) as string -> string, env
        | Expr.BOOL(_) as boolean -> boolean, env
        | Expr.ID(id) -> eval_impl env (lookup id env)

        | Expr.DUMMYLIST([Expr.NUMBER(_) as number]) -> number, env
        | Expr.DUMMYLIST([Expr.STRING(_) as string]) -> string, env
        | Expr.DUMMYLIST([Expr.BOOL(_) as boolean]) -> boolean, env
        | Expr.DUMMYLIST([Expr.ID(id)]) -> lookup id env, env

        | Expr.DUMMYLIST([Expr.OP(_) as op]) -> eval_impl env op

        | Expr.OP(op, t) when (Map.tryFind op numeric_operators).IsSome ->
            let (single_lambda, multiple_lambda) = Map.find op numeric_operators
            let evaluated_list, new_env = eval_args_num eval_impl env [] t
            match List.length evaluated_list with
            | 0 -> failwith "eval_impl@ operator + can't have 0 arguments"
            | 1 -> single_lambda (List.head evaluated_list), new_env
            | _ -> List.reduce (fun x y -> multiple_lambda (x, y)) evaluated_list, new_env
        | Expr.OP(op, t) when (Map.tryFind op bool_operators).IsSome ->
            let (single_lambda, multiple_lambda) = Map.find op bool_operators
            let evaluated_list, new_env = eval_args_bool eval_impl env [] t
            match List.length evaluated_list with
            | 0 -> failwith "eval_impl@ operator + can't have 0 arguments"
            | 1 -> single_lambda (List.head evaluated_list), new_env
            | _ -> List.reduce (fun x y -> multiple_lambda (x, y)) evaluated_list, new_env

        // strong binary operators
        | Expr.OP("=", t) ->
            match List.length t with
            | 2 ->
                let first::second::[] = t
                let eval_first, new_env = eval_impl env first
                let eval_second, new_env' = eval_impl new_env second
                (function 
                    | (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.BOOL(x = y)
                    | (Expr.BOOL(x), Expr.BOOL(y)) -> Expr.BOOL(x = y)
                    | (Expr.STRING(x), Expr.STRING(y)) -> Expr.BOOL(x = y)
                    | _ -> failwith ("eval_impl@ given unsupported arguments: " + (sprintf "%A %A" eval_first eval_second))
                ) (eval_first, eval_second), new_env'
            | _ -> failwith ("eval_impl@ operator " + (sprintf "%s" "=") + " can't have not 2 arguments")
        | Expr.OP(op, t) when (Map.tryFind op binary_operators).IsSome ->
            let evaluated_list, new_env = eval_args_num eval_impl env [] t
            let functor = Map.find op binary_operators
            match List.length evaluated_list with
            | 2 -> 
                let first::second::[] = evaluated_list
                functor (first, second), new_env
            | _ -> failwith ("eval_impl@ operator " + (sprintf "%s" op) + " can't have not 2 arguments")

        | Expr.LET(id, list) -> Expr.DUMMY (""), (Map.add id list env)
        | Expr.SET(id, list) -> Expr.DUMMY (""), (Map.add id list env)
        | Expr.COND(cond, expr1, expr2) ->
            let eval_cond, new_env = eval_impl env cond
            match eval_cond with 
            | Expr.NUMBER(n) -> if Convert.ToBoolean n then (expr1, new_env) else (expr2, new_env)
            | Expr.BOOL(b) -> if b then (expr1, new_env) else (expr2, new_env)
            | waste -> failwith ("eval_impl@ unevaluatable cond expression: " + (sprintf "%A" waste))
        | Expr.DUMMYLIST(list) -> //when List.forall (fun x -> match x with | Expr.DUMMYLIST(_) -> true | _ -> false) list ->
            let rec eval_lists env = function
                | h::t -> 
                    let evaluated_first, new_env = eval_impl env h
                    match evaluated_first with
                    | Expr.DUMMY(_) as dummy -> eval_lists new_env t
                    | _ ->
                        let evaluated, new_env' = eval_impl new_env evaluated_first // because there can be cond or let that does not eval exprs
                        match evaluated with
                        | Expr.NUMBER(_) | Expr.STRING(_) | Expr.BOOL(_)-> 
                            if List.length t <> 0 then printfn "eval_dummy_lists@ warning# useless members at the end of list" // should we treat tail?
                            evaluated, new_env'
                        | _ -> eval_lists new_env' t
                | [] ->
                    //printfn "eval_dummy_lists@ warning# no value given for evaluation"
                    Expr.DUMMY(""), env
            eval_lists env list
        | Expr.FUNC(id, args, body, _, arity) ->
            Expr.DUMMY(""), (Map.add id (Expr.FUNC(id, args, body, env, arity)) env)
        | Expr.CALL(id, Expr.DUMMYLIST(args), arity) ->
            let env_function = Map.tryFind id env
            if env_function.IsNone then failwith ("eval_impl@ use of undeclared function " + id)
            else 
                let (Expr.FUNC(_, Expr.DUMMYARGLIST(env_args), body, env_env, env_arity)) = env_function.Value
                if arity <> env_arity 
                then failwith ("eval_impl@ function use with different arity: expected " + (sprintf "%A" env_arity) + " got: " + (sprintf "%A" arity))
                else
                    let rec add_env_args env = function
                    | (Expr.DUMMYLIST([Expr.ID(h1)])::t1), (h2::t2) -> 
                        let eval_h2, new_env =  eval_impl env h2
                        add_env_args (Map.add h1 eval_h2 new_env) (t1, t2)
                    | ([], []) -> env
                    | waste -> failwith ("eval_impl@ Some serious thing happened diring concatenations of maps: " + (sprintf "%A" waste))

                    let new_env = add_env_args env (env_args, args) 
                    let merged_env = Map.fold (fun acc key value -> Map.add key value acc) env new_env
                    let merged_env2 = Map.fold (fun acc key value -> Map.add key value acc) merged_env env_env

                    eval_impl merged_env2 body
        | Expr.PRINT(body) ->
            let evaludated_body, new_env = eval_impl env body
            match evaludated_body with
            | Expr.DUMMY(_) -> failwith ("eval_impl.print@ unevaluatable dummy value to print\n")
            | _ ->
                let evaluated_eval, new_env' = eval_impl new_env evaludated_body
                match evaluated_eval with
                | Expr.NUMBER(num) -> 
                    printfn "%f" num
                    DUMMY(""), new_env
                | Expr.STRING(str) -> 
                    printfn "\"%s\"" str
                    DUMMY(""), new_env
                | Expr.BOOL(boolean) ->
                    if boolean then
                        printfn "true"
                        DUMMY(""), new_env
                    else
                        printfn "false"
                        DUMMY(""), new_env
                | Expr.ID(id_val) as id ->
                    let evaluated_id, new_env'' = eval_impl new_env' id
                    printfn "id: %s = %A" id_val evaluated_id
                    DUMMY(""), new_env''
        | Expr.DUMMY(_) -> failwith ("eval_impl@ dummy value is invaluatable\n")
        | waste -> failwith ("eval_impl@ wrong structure to evaluate" + (sprintf "%A\n" waste))

    match ast with
    | h -> eval_impl env h

// Runtime part

let env = Map<string, Expr> []

let rec repl env =
    printf "> "
    let source = Console.ReadLine()
    try
        printfn "Source: %s" source
        let tokens = tokenize (source |> Seq.toList)
        let expr = parse tokens
        printfn "parsed: %A" expr
        let evaluated, new_env = eval env expr
        printfn "Evaluated: %A\n" evaluated
        //printfn "Env: %A\n" new_env
        repl new_env
    with ex ->
        printfn "Error: %s" ex.Message
        repl env

let test env = 
    let test_impl source =
        try
            printfn "Source: %s" source
            let tokens = tokenize ("(" + source + ")" |> Seq.toList)
            let expr = parse tokens
            let evaluated, new_env = eval env expr
            printfn "Evaluated: %A\n" evaluated
            //printfn "Env: %A\n" new_env
            ()
        with ex ->
            printfn "Error: %s\n" ex.Message
    test_impl "(1)"
    test_impl "(\"string\")"
    test_impl "(+ 1 2)"
    test_impl "(+ 1.2 2.4)"
    test_impl "(1)"
    test_impl "(\"string\")"
    test_impl "(let id 1)"
    test_impl "((let id 1) (+ id 1))"
    test_impl "((let id ( + 1 2 ) ) id)"
    test_impl "(if (1) then (2) else (3))"
    test_impl "(let id (if 1 then 2 else 3))"
    test_impl "((let id 1) (+ id 1))"
    test_impl "((let id_1 1) (let id_2 2) (+ id_1 id_2))"
    test_impl "(makefun id_1 {arg_1 arg_2 } ( + arg_1 arg_2 ))"
    test_impl "((makefun id_1 {arg_1 arg_2} ( + arg_1 arg_2 ) ) (id_1 1 (if 0 then 15 else 8 )))"
    test_impl "((makefun id_1 {arg_1 arg_2} ( + arg_1 arg_2 ) ) (let id_for_func 7) ( id_1 id_for_func (if 0 then 15 else 8)))"
    test_impl "((makefun id_1 {arg_1 arg_2} ( + arg_1 arg_2 ) ) (let arg_2 7) (id_1 2 1))"
    test_impl "((let closure_id 8 ) (makefun id_1 {arg_1 arg_2} (+ arg_1 arg_2 closure_id)) (let id_for_func 7) (id_1 id_for_func (if 0 then 15 else 8)))"
    test_impl "((makefun id_1 {arg_1} ( if ( = arg_1 1 ) then arg_1 else (* ( id_1 ( - arg_1 1 ) ) arg_1 ) ) ) ( id_1 5 ))"
    test_impl "((makefun id_1 {} (if true then 1 else false)) (id_1))"
    test_impl "((makefun id_1 {a} (if (> a 1) then ((print a) (id_1 (- a 1))) else \"function ended\")) (id_1 5))"
    test_impl "(let id (+ 1 2)))"
    test_impl "(let id (+ 1 2)))"
    test_impl "(if (| true 1) then 1 else 2))"
    test_impl "(if (& true (< 1 0)) then 1 else 2)"
    test_impl "((let id 1) (makefun arg {} (+ id 1)) (arg))"
    test_impl "((let id 1) (print id) (set id 2) (print id))"

let rec file env filename =
    let lines = File.ReadAllLines(filename)
    let source = String.concat "" lines
    try
        printfn "Source: %s" source
        let tokens = tokenize (source |> Seq.toList)
        let expr = parse tokens
        let evaluated, new_env = eval env expr
        printfn "Evaluated: %A\n" evaluated
        repl env
    with ex ->
        printfn "Error: %s" ex.Message
        repl env

//repl env
//test env
//file filename env source.kva