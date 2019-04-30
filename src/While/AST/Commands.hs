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

    evalCmd :: Cmd -> State -> Maybe State
    evalCmd cmd state = case cmd of
        Skip -> Just state
        Assign var expr -> case evalArith expr state of
            value@(Just _) -> Just $ setVar var value state
            Nothing -> Nothing
        Seq cmd1 cmd2 -> case evalCmd cmd1 state of
            Just state' -> evalCmd cmd2 state'
            Nothing -> Nothing
        If expr cmd1 cmd2 -> case evalBool expr state of
            Just True -> evalCmd cmd1 state
            Just False -> evalCmd cmd2 state
            Nothing -> Nothing
        While expr body -> case evalBool expr state of
            Just True -> case evalCmd body state of
                -- How to handle infinite loops?
                --
                -- Obviously, we can't solve the halting problem, but we can handle the simplest case,
                -- ie. where a while (true) loop has no side effects (ie. state changes) and thus would,
                -- if implemented naively, recurse infinitely.
                --
                -- So, how do we handle this? There are two solutions:
                --  1) return Just state (technically the state never changes. but...)
                --  2) return Nothing    (the loop never terminates, so it actually can't return anything)
                --
                -- strictly speaking 2) is correct. 1) is also sort of correct, in the sense that we know what
                -- the final state is at the end of executing the while loop, IF it terminated (whci it does not).
                --
                -- For unittesting purposes I could define this either way, but given that I've already stated
                -- that a program with missing variable references is invalid, it makes sense to say that a program
                -- that obviously doesn't terminate is also invalid.
                --
                Just state' -> if state' /= state
                    then evalCmd cmd state'      -- state has changed, valid loop
                    else Nothing                 -- state has not changed, invalid loop 
                Nothing -> Nothing
            Just False -> Just state
            Nothing -> Nothing

    simplifyCmd :: Cmd -> Cmd
    simplifyCmd cmd@Skip = cmd
    simplifyCmd (Seq Skip cmd) = simplifyCmd cmd
    simplifyCmd (Assign var expr) = Assign var $ simplifyArith expr
    simplifyCmd (If cond cmd1 cmd2) = case simplifyBool cond of
        BTrue  -> simplifyCmd cmd1
        BFalse -> simplifyCmd cmd2
        cond'  -> If cond' (simplifyCmd cmd1) (simplifyCmd cmd2)
    simplifyCmd (While cond body) = case simplifyBool cond of
        BFalse -> Skip
        cond'  -> While cond' (simplifyCmd body)

    weakCheckIsNonterminating :: Cmd -> Bool
    weakCheckIsNonterminating cmd = check . simplifyCmd $ cmd
        where
            check (While BTrue _) = True
            check (Seq cmd1 cmd2) = (check cmd1) || (check cmd2)
            check _ = False

    isValidProgram :: Cmd -> State -> Bool
    isValidProgram cmd state = case evalCmd cmd state of
        Nothing -> False
        otherwise -> True
