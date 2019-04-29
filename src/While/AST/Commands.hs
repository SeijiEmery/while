module While.AST.Commands
where
    import While.Types
    import While.AST.Arithmetic
    import While.AST.Boolean

    data Cmd
        = Skip
        | Assign Variable AExpr
        | Seq Cmd Cmd
        | If BExpr Cmd Cmd
        | While BExpr Cmd
        deriving (Show, Eq)

    eval_cmd :: Cmd -> State -> State
    eval_cmd cmd state = case cmd of
        Skip -> state
        Assign var expr -> 
            let value = eval_arith expr state
            in setVar var value state
        Seq cmd1 cmd2 -> 
            let state' = eval_cmd cmd1 state
            in eval_cmd cmd2 state'
        If expr cmd1 cmd2 -> case eval_bool expr state of
            True -> eval_cmd cmd1 state
            False -> eval_cmd cmd2 state
        While expr body -> case eval_bool expr state of
            True ->
                let state' = eval_cmd body state
                in eval_cmd cmd state
            False -> state