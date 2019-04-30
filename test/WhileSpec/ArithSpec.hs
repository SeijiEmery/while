module WhileSpec.ArithSpec (testArithExprs) where
    import While.AST.Arithmetic
    import While.Types
    import Test.HUnit

    testArithExprs = test arithTests
    arithTests = 
        [ testEval (Const 10) [] $ Just 10
        , testEval (Const $ -1) [] $ Just $ -1
        , testEval (Var "x") [] $ Nothing
        , testEval (Var "x") [("x", 10)] $ Just 10
        , testEval (Var "y") [("x", 10)] $ Nothing 
        , testEval (Var "y") [("x", 10), ("y", 12)] $ Just 12
        , testEval (Add (Const 10) (Const 4)) [] $ Just 14
        , testEval (Add (Const 10) (Var "x")) [] $ Nothing
        , testEval (Add (Var "x") (Const 10)) [] $ Nothing
        , testEval (Add (Var "x") (Const 10)) [("xyz", 10)] $ Nothing
        , testEval (Add (Var "x") (Const 10)) [("x", 10)] $ Just 20
        , testEval (Sub (Var "x") (Var "y")) [("y", 10), ("x", 22)] $ Just 12
        , testEval (Sub (Var "x") (Var "y")) [("y", 10), ("z", 22)] $ Nothing
        , testEval (Mul (Var "x") (Var "y")) [("y", 22), ("x", -1)] $ Just $ -22
        , testEval (Mul (Var "x") (Var "y")) [("y", 22), ("z", -1)] $ Nothing
        , testEval (Mul (Add (Const 3) (Const 2)) (Sub (Var "a") (Const 3))) [("a", 1), ("b", 22)] $ Just $ -10
        , testEval (Mul (Add (Const 3) (Mul (Var "z") (Const 22))) (Sub (Const 3) (Var "a"))) [("a", 1), ("b", 22)] $ Nothing
        ]
        where 
            testEval :: AExpr -> [(Variable, Value)] -> Maybe Value -> Test
            testEval expr vars expected = 
                let result = evalArith expr $ fromList vars
                in result ~?= expected
