module While.Types
    ( Variable
    , Value
    , State
    , get_var
    , set_var
    , emptyState
) where
import Data.Map.Strict as Map

type Variable = String
type Value = Integer
type State = Map.Map Variable Value

get_var :: Variable -> State -> Maybe Value
get_var = Map.lookup

set_var :: Variable -> Maybe Value -> State -> State
set_var var (Just value) = Map.insert var value
set_var var Nothing = Map.delete var

emptyState = Map.empty
