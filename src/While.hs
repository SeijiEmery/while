{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module While where
import qualified Data.Map.Strict as Map
import Control.Monad

type Variable = String
type Value = Integer
type State = Map.Map Variable Value


class Show a => Eval a b where
    eval :: a -> State -> b

data AExpr
    = Const Value
    | Var Variable
    | Add AExpr AExpr
    | Sub AExpr AExpr
    | Mul AExpr AExpr
    deriving (Show)

instance Eval AExpr (Maybe Value) where
    eval expr state = case expr of
        Const x -> Just x
        Var x -> Map.lookup x state
        Add a b -> apply (+) (eval' a) (eval' b)
        Sub a b -> apply (-) (eval' a) (eval' b)
        Mul a b -> apply (*) (eval' a) (eval' b)
        where 
            apply = liftM2
            eval' a = eval a state

data BExpr
    = BTrue
    | BFalse
    | Equal AExpr AExpr
    | Less AExpr AExpr
    | Not BExpr
    | And BExpr BExpr
    | Or BExpr BExpr
    deriving (Show)

instance Eval BExpr Bool where
    eval expr state = case expr of
        BTrue -> True
        BFalse -> False
        Equal a b -> apply_arith (==) a b
        Less a b -> apply_arith (<) a b
        Not a -> not (eval' a)

        -- short circuiting
        And a b -> case (eval' a) of
            False -> False
            True -> eval' b
        Or a b -> case (eval' a) of
            True -> True
            False -> eval' b
        where
            eval_arith :: AExpr -> Value
            eval_arith a = eval a state

            eval' a = eval a state

            apply_bool :: (Bool -> Bool -> Bool) -> BExpr -> BExpr -> Bool
            apply_bool f a b = f (eval' a) (eval' b)

            apply_arith :: (Value -> Value -> Bool) -> AExpr -> AExpr -> Bool
            apply_arith f a b = f (eval_arith a) (eval_arith b)

data Cmd
    = Skip
    | Assign Variable AExpr
    | Seq Cmd Cmd
    | If BExpr Cmd Cmd
    | While BExpr Cmd
    deriving (Show)

instance Eval Cmd State where
    eval cmd state = case cmd of
        Skip -> state
        Assign var expr -> case eval expr state of
            Just a -> Map.insert var a state
            Nothing -> Map.delete var state
        Seq cmd1 cmd2 -> 
            let state' = eval cmd1 state
            in eval cmd2 state'
        If expr cmd1 cmd2 -> case eval expr state of
            True -> eval cmd1 state
            False -> eval cmd2 state
        While expr body -> case eval expr state of
            True ->
                let state' = eval body state
                in eval cmd state
            False -> state
