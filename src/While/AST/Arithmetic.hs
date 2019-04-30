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

    simplify :: AExpr -> AExpr
    simplify expr@(Const k) = expr
    simplify expr@(Var x) = expr
    
    simplify (Add (Const k1) (Const k2)) = Const (k1 + k2)
    simplify (Sub (Const k1) (Const k2)) = Const (k1 - k2)
    simplify (Mul (Const k1) (Const k2)) = Const (k1 * k2)

    simplify (Add (Const k1) (Add (Const k2) expr)) = simplify $ Add (Const (k1 + k2)) (simplify expr)
    simplify (Sub (Const k1) (Sub (Const k2) expr)) = simplify $ Add (Const (k1 - k2)) (simplify expr)
    simplify (Mul (Const k1) (Mul (Const k2) expr)) = simplify $ Mul (Const (k1 * k2)) (simplify expr)

    simplify (Add (Const 0) expr) = simplify expr
    simplify (Sub (Const 0) (Sub expr1 expr2)) = simplify $ Add (simplify expr1) (simplify expr2)
    simplify (Mul (Const 1) expr) = simplify expr

    simplify (Add expr (Const k)) = simplify $ Add (Const k) expr
    simplify (Sub expr (Const k)) = simplify $ Sub (Const k) expr
    simplify (Mul expr (Const k)) = simplify $ Mul (Const k) expr

    simplify expr@(Sub expr1 expr2)
        | expr1 == expr2 = (Const 0)
        | otherwise      =
            let expr' = (Sub (simplify expr1) (simplify expr2))
            in if expr' /= expr then simplify expr' else expr'

    simplify expr@(Add expr1 expr2)
        | expr1 == expr2 = simplify (Mul (Const 2) (simplify expr1))
        | otherwise      = 
            let expr' = (Add (simplify expr1) (simplify expr2))
            in if expr' /= expr then simplify expr' else expr'

    simplify expr@(Mul expr1 expr2) = 
        let expr' = (Mul (simplify expr1) (simplify expr2))
        in if expr' /= expr then simplify expr' else expr'



