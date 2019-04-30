import Test.HUnit
import WhileSpec.ArithSpec
import WhileSpec.BoolSpec

runTests = runTestTT . test $ tests
    where tests =   [ testArithExprs
                    , testBoolExprs
                    ]

main :: IO ()
main = do 
    count <- runTests
    putStrLn ""
