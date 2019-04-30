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


