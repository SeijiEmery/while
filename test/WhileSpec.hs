import Test.HUnit
import While (
    AExpr (Const), eval_arith, emptyState)

tests = test [
    "const tests" ~: "baz" ~:
        Just 12 ~=? eval_arith (Const 10) emptyState]




-- tests = TestList 
--     [ TestLabel "test arithmetic expressions" test_arith
--     , TestLabel "test boolean expressions" test_bool
--     , TestLabel "test commands" test_cmd
--     ]

main :: IO ()
main = do 
    count <- runTestTT tests
    putStrLn ""
