module While.AST.Boolean where
    import While.Types
    import While.AST.Arithmetic
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


    simplifyBool :: BExpr -> BExpr
    simplifyBool BTrue = BTrue
    simplifyBool BFalse = BFalse
    simplifyBool (Not BFalse) = BTrue 
    simplifyBool (Not BTrue) = BFalse
    simplifyBool (Not (Not expr)) = simplifyBool expr
    simplifyBool (And BTrue expr) = simplifyBool expr
    simplifyBool (And BFalse expr) = BFalse
    simplifyBool (Or BTrue expr) = BTrue
    simplifyBool (Or BFalse expr) = simplifyBool expr

    simplifyBool expr@(Equal (Var x1) (Var x2))
        | x1 == x2 = BTrue
        | otherwise = expr

    simplifyBool expr@(Less (Var x1) (Var x2))
        | x1 == x2 = BFalse
        | otherwise = expr

    simplifyBool expr@(Equal expr1 expr2) = expr
        -- | (hasFreeVars expr1') || (hasFreeVars expr2') = result
        -- | otherwise = evalBool expr
        -- where
        --     result = if expr == expr' then expr' else simplifyBool expr'
        --     expr' = Equal expr1' expr2'
        --     expr1' = simplifyArith expr1
        --     expr2' = simplifyArith expr2

    simplifyBool expr@(Less expr1 expr2) = expr
        -- | (hasFreeVars expr1') || (hasFreeVars expr2') = result
        -- | otherwise = evalBool expr
        -- where
        --     result = if expr == expr' then expr' else simplifyBool expr'
        --     expr' = Less expr1' expr2'
        --     expr1' = simplifyArith expr1
        --     expr2' = simplifyArith expr2
