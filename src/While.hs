module While where
import qualified Data.Map.Strict as Map

type Variable = String
type Value = Integer
type State = Map.Map Variable Value
-- type State = [(Variable, Value)]
type Result = (State, Value)

data AExpr 
    = Const Value
    | Var Variable
    | Add AExpr AExpr
    | Sub AExpr AExpr
    | Mul AExpr AExpr
    deriving (Show)

apply_binary :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
apply_binary f Nothing _ = Nothing
apply_binary f _ Nothing = Nothing
apply_binary f (Just a) (Just b) = Just $ f a b
apply_binary_eval eval f a b = apply_binary f (eval a) (eval b)

eval_arith :: State -> AExpr -> Maybe Value
eval_arith state expr = case expr of
    Const x -> Just x
    Var x -> Map.lookup x state
    Add a b -> apply (+) a b
    Sub a b -> apply (-) a b
    Mul a b -> apply (*) a b
    where apply = apply_binary_eval $ eval_arith state

data BExpr
    = BTrue
    | BFalse
    | Equal AExpr AExpr
    | Less AExpr AExpr
    | Not BExpr
    | And BExpr BExpr
    | Or BExpr BExpr
    deriving (Show)

eval_bool :: State -> BExpr -> Bool
eval_bool state expr = case expr of
    BTrue -> True
    BFalse -> False
    Equal a b -> (evala a) == (evala b)
    Less a b -> (evala a) < (evala b)
    Not a -> not (eval a)
    And a b -> case (eval a) of -- short circuiting and
        False -> False
        True -> eval b
    Or a b -> case (eval b) of -- short circuiting or
        True -> True
        False -> eval b
    where 
        eval = eval_bool state
        evala = eval_arith state

data Cmd
    = Skip
    | Assign Variable AExpr
    | Seq Cmd Cmd
    | If BExpr Cmd Cmd
    | While BExpr Cmd
    deriving (Show)

eval_cmd :: State -> Cmd -> State
eval_cmd state cmd = case cmd of
    Skip -> state
    Assign var expr -> case eval_arith state expr of
        Just a -> Map.insert var a state
        Nothing -> state
    Seq cmd1 cmd2 -> eval_cmd (eval_cmd state cmd1) cmd2
    If expr cmd1 cmd2 -> case eval_bool state expr of
        True -> eval_cmd state cmd1
        False -> eval_cmd state cmd2
    While expr cmd -> case eval_bool state expr of
        True -> eval_cmd (eval_cmd state cmd) (While expr cmd)
        False -> state

