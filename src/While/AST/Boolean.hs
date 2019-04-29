module While.AST.Boolean where
    import While.Types (State, Value)
    import While.AST.Arithmetic as Arith
    import Control.Monad

    -- type AExpr = Arith.AExpr
    data BExpr
        = BTrue
        | BFalse
        | Equal AExpr AExpr
        | Less AExpr AExpr
        | Not BExpr
        | And BExpr BExpr
        | Or BExpr BExpr
        deriving (Show)

    eval_bool :: BExpr -> State -> Bool
    eval_bool expr state = case expr of
        BTrue -> True
        BFalse -> False
        Equal a b -> apply_arith maybe_equality a b
        Less  a b -> apply_arith maybe_less a b
        Not a -> not (eval' a)

        -- short circuiting
        And a b -> case (eval' a) of
            False -> False
            True -> eval' b
        Or a b -> case (eval' a) of
            True -> True
            False -> eval' b
        where
            eval' a = eval_bool a state
            apply_arith f a b = f a' b'
                where 
                    a' = eval_arith a state 
                    b' = eval_arith b state

            maybe_equality :: Maybe Value -> Maybe Value -> Bool
            maybe_equality (Just a) (Just b) = a == b
            maybe_equality Nothing Nothing = True
            maybe_equality _ _ = False

            maybe_less :: Maybe Value -> Maybe Value -> Bool
            maybe_less (Just a) (Just b) = a < b
            maybe_less _ _ = False
