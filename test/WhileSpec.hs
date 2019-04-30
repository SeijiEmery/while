import Test.HUnit
import WhileSpec.ArithSpec
import WhileSpec.BoolSpec
import WhileSpec.CommandSpec

runTests = runTestTT . test $ tests
    where tests =   [ testArithExprs
                    , testBoolExprs
                    , testCmds
                    ]

main :: IO ()
main = do 
    count <- runTests
    putStrLn ""
