module WhileSpec.BoolSpec (testBoolExprs) where
    import While.AST.Boolean
    import While.AST.Arithmetic
    import While.Types
    import Test.HUnit

    testBoolExprs = test $ "boolean tests" ~: (test tests)
        where
            tests = [ boolTests
                , compareTests
                , varTests
                , undefinedVarTests
                -- , compoundTests
                ]

            eval :: BExpr -> [(Variable, Value)] -> Maybe Bool
            eval expr vars = evalBool expr $ fromList vars

            testEval :: BExpr -> [(Variable, Value)] -> Maybe Bool -> Test
            testEval expr vars expected = 
                let result = evalBool expr $ fromList vars
                in result ~?= expected

            -- Constants And Boolean functions
            boolTests = test $ "boolean tests" ~: (test tests)
                where tests =   [ eval BTrue [] ~?= Just True
                                , eval BFalse [] ~?= Just False
                                , eval (Not BTrue) [] ~?= Just False
                                , eval (Not BFalse) [] ~?= Just True
                                , eval (And BTrue BTrue) [] ~?= Just True
                                , eval (And BTrue BFalse) [] ~?= Just False
                                , eval (And BFalse BTrue) [] ~?= Just False
                                , eval (And BFalse BFalse) [] ~?= Just False
                                , eval (Or BTrue BTrue) [] ~?= Just True
                                , eval (Or BTrue BFalse) [] ~?= Just True
                                , eval (Or BFalse BTrue) [] ~?= Just True
                                , eval (Or BFalse BFalse) [] ~?= Just False
                                ]

            -- test values
            a = (-1)
            b = 0
            c = 2

            a' = Const a
            b' = Const b
            c' = Const c

            -- c'ompare values
            compareTests = test $ "comparison" ~: (test tests)
                where tests =   [ eval (Equal a' a') [] ~?= Just True
                                , eval (Equal b' b') [] ~?= Just True
                                , eval (Equal c' c') [] ~?= Just True

                                , eval (Equal a' b') [] ~?= Just False
                                , eval (Equal b' a') [] ~?= Just False
                                , eval (Equal a' c') [] ~?= Just False
                                , eval (Equal c' a') [] ~?= Just False
                                , eval (Equal b' c') [] ~?= Just False
                                , eval (Equal c' b') [] ~?= Just False

                                , eval (Less a' a') [] ~?= Just False
                                , eval (Less b' b') [] ~?= Just False
                                , eval (Less c' c') [] ~?= Just False

                                , eval (Less a' b') [] ~?= Just True
                                , eval (Less b' a') [] ~?= Just False
                                , eval (Less a' c') [] ~?= Just True
                                , eval (Less c' a') [] ~?= Just False
                                , eval (Less b' c') [] ~?= Just True
                                , eval (Less c' b') [] ~?= Just False
                                ]

            -- test variables
            x' = (Var "x")
            y' = (Var "y")

            -- Compare variables
            varTests = test $ "defined variables" ~: (test tests)
                where tests =   [ eval (Equal a' x') [("x", a)] ~?= Just True
                                , eval (Equal x' a') [("x", a)] ~?= Just True
                                , eval (Equal a' x') [("x", b)] ~?= Just False
                                , eval (Equal x' a') [("x", b)] ~?= Just False
                                , eval (Equal x' y') [("x", a), ("y", a)] ~?= Just True
                                , eval (Equal x' y') [("x", a), ("y", b)] ~?= Just False

                                , eval (Less x' b') [("x", a)] ~?= Just True
                                , eval (Less b' x') [("x", a)] ~?= Just False
                                , eval (Less x' y') [("x", a), ("y", b)] ~?= Just True
                                , eval (Less y' x') [("x", a), ("y", b)] ~?= Just False
                                ]

            -- expression result is undefined if Any of the variables in it Are undefined
            undefinedVarTests = test $ "undefined variables" ~: (test tests)
                where tests =   [ eval (Equal x' a') [] ~?= Nothing
                                , eval (Equal a' x') [] ~?= Nothing
                                , eval (Equal x' y') [] ~?= Nothing

                                , eval (Less x' a') [] ~?= Nothing
                                , eval (Less a' x') [] ~?= Nothing
                                , eval (Less x' y') [] ~?= Nothing
                                ]
            
