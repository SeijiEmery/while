module While where

data Variable = String
data Value = Integer
data State = Map Variable Value
data Result = { state :: State value :: Value }

data AExpr 
    = Const Value
    | Var Variable
    | Add AExpr AExpr
    | Sub AExpr AExpr
    | Mul AExpr AExpr
    deriving (Show)

data BExpr
    = Const Boolean
    | Equal AExpr AExpr
    | Less AExpr AExpr
    | Not AExpr AExpr
    | And BExpr BExpr
    | Or BExpr BExpr
    deriving (Show)

data Cmd
    = Skip
    | Assign Variable AExpr
    | Seq Cmd Cmd
    | If BExpr Cmd Cmd
    | While BExpr Cmd
    deriving (Show)
