module While.AST.Arithmetic where
    import While.Types
    import Control.Monad

    data AExpr
        = Const Value
        | Var Variable
        | Add AExpr AExpr
        | Sub AExpr AExpr
        | Mul AExpr AExpr
        deriving (Show, Eq)

    evalArith :: AExpr -> State -> Maybe Value
    evalArith expr state = case expr of
        Const x -> Just x
        Var x -> getVar x state
        Add a b -> apply (+) (eval' a) (eval' b)
        Sub a b -> apply (-) (eval' a) (eval' b)
        Mul a b -> apply (*) (eval' a) (eval' b)
        where 
            apply = liftM2
            eval' a = evalArith a state

    hasFreeVars :: AExpr -> Bool
    hasFreeVars (Const _) = False
    hasFreeVars (Var _) = True
    hasFreeVars (Add expr1 expr2) = (hasFreeVars expr1) || (hasFreeVars expr2)
    hasFreeVars (Sub expr1 expr2) = (hasFreeVars expr1) || (hasFreeVars expr2)
    hasFreeVars (Mul expr1 expr2) = (hasFreeVars expr1) || (hasFreeVars expr2)

    simplifyArith :: AExpr -> AExpr
    simplifyArith expr@(Const k) = expr
    simplifyArith expr@(Var x) = expr

    simplifyArith (Add (Const k1) (Const k2)) = Const (k1 + k2)
    simplifyArith (Sub (Const k1) (Const k2)) = Const (k1 - k2)
    simplifyArith (Mul (Const k1) (Const k2)) = Const (k1 * k2)

    simplifyArith (Add (Const k1) (Add (Const k2) expr)) = simplifyArith $ Add (Const (k1 + k2)) (simplifyArith expr)
    simplifyArith (Sub (Const k1) (Sub (Const k2) expr)) = simplifyArith $ Add (Const (k1 - k2)) (simplifyArith expr)
    simplifyArith (Mul (Const k1) (Mul (Const k2) expr)) = simplifyArith $ Mul (Const (k1 * k2)) (simplifyArith expr)

    simplifyArith (Add (Const 0) expr) = simplifyArith expr
    simplifyArith (Sub (Const 0) (Sub expr1 expr2)) = simplifyArith $ Add (simplifyArith expr1) (simplifyArith expr2)
    simplifyArith (Mul (Const 1) expr) = simplifyArith expr

    simplifyArith (Add expr (Const k)) = simplifyArith $ Add (Const k) expr
    simplifyArith (Sub expr (Const k)) = simplifyArith $ Sub (Const k) expr
    simplifyArith (Mul expr (Const k)) = simplifyArith $ Mul (Const k) expr

    simplifyArith expr@(Sub expr1 expr2)
        | expr1 == expr2 = (Const 0)
        | otherwise      =
            let expr' = (Sub (simplifyArith expr1) (simplifyArith expr2))
            in if expr' /= expr then simplifyArith expr' else expr'

    simplifyArith expr@(Add expr1 expr2)
        | expr1 == expr2 = simplifyArith (Mul (Const 2) (simplifyArith expr1))
        | otherwise      = 
            let expr' = (Add (simplifyArith expr1) (simplifyArith expr2))
            in if expr' /= expr then simplifyArith expr' else expr'

    simplifyArith expr@(Mul expr1 expr2) = 
        let expr' = (Mul (simplifyArith expr1) (simplifyArith expr2))
        in if expr' /= expr then simplifyArith expr' else expr'
