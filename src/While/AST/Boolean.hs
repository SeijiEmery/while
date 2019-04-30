module While.AST.Boolean where
    import While.Types
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
        deriving (Show, Eq)

    evalBool :: BExpr -> State -> Maybe Bool
    evalBool expr state = case expr of
        BTrue -> Just True
        BFalse -> Just False
        Not a -> liftM (not) (eval' a)
        And a b -> liftM2 (&&) (eval' a) (eval' b)
        Or a b -> liftM2 (||) (eval' a) (eval' b)
        Equal a b -> liftM2 (==) (eval'' a) (eval'' b)
        Less a b -> liftM2 (<) (eval'' a) (eval'' b)
        where
            eval' a = evalBool a state
            eval'' a = evalArith a state
