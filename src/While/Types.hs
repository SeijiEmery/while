module While.Types where
    import Data.Map.Strict as Map

    type Variable = String
    type Value = Integer
    type State = Map.Map Variable Value

    getVar :: Variable -> State -> Maybe Value
    getVar = Map.lookup

    setVar :: Variable -> Maybe Value -> State -> State
    setVar var (Just value) = Map.insert var value
    setVar var Nothing = Map.delete var

    emptyState :: State
    emptyState = Map.empty

    fromList :: [(Variable, Value)] -> State
    fromList = Map.fromList
