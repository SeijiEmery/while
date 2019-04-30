module WhileSpec.CommandSpec (testCmds) where
    import While.AST.Commands
    import While.AST.Boolean
    import While.AST.Arithmetic
    import While.Types
    import Test.HUnit

    testCmds = test $ "command tests" ~: (test tests)
        where
            tests = [ testSkip
                , testAssign
                , testSeq
                , testIf
                , testWhile
                , testGCD
                ]

             -- test variables

            x = "x"
            y = "y"
            x' = (Var x)
            y' = (Var y)

            -- test values
            a = (-1)
            b = 0
            c = 2

            a' = Const a
            b' = Const b
            c' = Const c

            xa = [(x, a)]
            xb = [(x, b)]
            xc = [(x, c)]
            ya = [(y, a)]
            yb = [(y, b)]
            state vars = Just $ fromList vars

            eval :: Cmd -> [(Variable, Value)] -> Maybe While.Types.State
            eval expr vars = evalCmd expr $ fromList vars

            testSkip = test tests
                where
                    tests = [ eval Skip [] ~?= state []
                            , eval Skip xa ~?= state xa
                            ]

            testAssign = test tests
                where
                    tests = [ eval (Assign x a') [] ~?= state xa
                            , eval (Assign x a') xb ~?= state xa
                            , eval (Assign x a') yb ~?= state (xa ++ yb)
                            , eval (Assign x y') yb ~?= state (xb ++ yb)

                            -- These expressions are invalid as they're referencing undefined variables
                            , eval (Assign x y') [] ~?= Nothing
                            , eval (Assign x y') xa ~?= Nothing
                            , eval (Assign x x') [] ~?= Nothing
                            ]

            testSeq = test tests
                where
                    tests = [ eval (Seq (Assign x a') (Assign x b')) [] ~?= state xb
                            , eval (Seq (Assign x a') (Assign y b')) [] ~?= state (xa ++ yb)
                            , eval (Seq (Assign y b') (Assign x a')) [] ~?= state (xa ++ yb)

                            -- invalid if anything is referencing undefined variables
                            -- order matters though:
                            , eval (Seq (Assign x y') (Assign y x')) [] ~?= Nothing
                            , eval (Seq (Assign x y') (Assign y x')) xa ~?= Nothing
                            , eval (Seq (Assign x y') (Assign y x')) ya ~?= state (xa ++ ya)
                            ]

            testIf = test tests
                where
                    tests = [ eval (If BTrue (Assign x a') (Assign x b')) [] ~?= state xa
                            , eval (If BFalse (Assign x a') (Assign x b')) [] ~?= state xb

                            -- again, invalid if undeclared variables are used
                            , eval (If (Equal x' c') (Assign x a') (Assign x b')) xc ~?= state xa
                            , eval (If (Equal x' c') (Assign x a') (Assign x b')) xa ~?= state xb
                            , eval (If (Equal x' c') (Assign x a') (Assign x b')) [] ~?= Nothing
                            ]

            testWhile = test tests
                where
                    tests = [ eval (While BFalse (Assign x a')) [] ~?= state []
                            , eval (While (Equal x' a') (Assign x b')) xa ~?= state xb

                            -- infinite loop is invalid as it will never terminate
                            , eval (While BTrue (Assign x a')) [] ~?= Nothing
                            , eval (While (Equal x' a') (Assign x a')) xa ~?= Nothing

                            -- if we have undefined variables in the while loop, that isn't valid either
                            , eval (While (Equal x' a') (Assign x b')) [] ~?= Nothing
                            , eval (While (Equal x' a') (Seq (Assign y x') (Assign x b'))) yb ~?= Nothing

                            -- proof that this works if x is defined:
                            , eval (While (Equal x' a') (Seq (Assign y x') (Assign x b'))) (xa ++ yb) ~?= state (xb ++ ya)
                            , eval (While (Equal x' a') (Seq (Assign y x') (Assign x b'))) xa ~?= state (xb ++ ya)
                            ]

            -- less-trivial tests using Euclid's GCD algorithm:
            evalGCD :: Value -> Value -> Maybe Value
            evalGCD a b = case eval gcd [("a", a), ("b", b)] of
                Nothing -> Nothing
                Just state' -> getVar "a" state'
                where gcd = 
                        (While (Not (Equal (Var "a") (Var "b")))
                            (If (Less (Var "b") (Var "a"))
                                (Assign "a" (Sub (Var "a") (Var "b")))
                                (Assign "b" (Sub (Var "b") (Var "a")))
                            )
                        )

            testGCD = test tests
                where
                    tests = [ evalGCD 10 2 ~?= Just 2
                            , evalGCD 1024 96 ~?= Just 32
                            , evalGCD 96 1024 ~?= Just 32
                            ]
