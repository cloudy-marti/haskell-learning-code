import Test.HUnit
import qualified Data.Algorithm.SatSolver.Clause as Clause
import qualified Data.Algorithm.SatSolver.Lit    as Lit
import qualified Data.Algorithm.SatSolver.Var    as Var

------------------------------------------- IsEmpty TEST -------------------------------------------------------
testNewClauseIsEmpty :: Test
testNewClauseIsEmpty = 
    TestCase $ assertEqual "Should return True for Clause.mk []" True (Clause.isEmpty c)
                           where 
                               c = Clause.mk [] :: Clause.Clause Int

testClauseIsNotEmpty :: Test
testClauseIsNotEmpty = 
    TestCase $ assertEqual "Should not return True for Clause.mk [some literal]" False (Clause.isEmpty c)
                           where 
                               c = Clause.mk [Lit.mkPos' 'a']

------------------------------------------- IsUnit TEST -------------------------------------------------------

testClauseIsUnitWithEmptyList :: Test
testClauseIsUnitWithEmptyList = 
    TestCase $ assertEqual "Should not return True for Clause.mk []" False (Clause.isUnit c)
                           where 
                               c = Clause.mk [] :: Clause.Clause Int

testClauseIsUnitWithOneLiteral:: Test
testClauseIsUnitWithOneLiteral = 
    TestCase $ assertEqual "Should return True for Clause.mk [one literal]" True (Clause.isUnit c)
                           where 
                               c = Clause.mk [Lit.mkPos' 'a']

testClauseIsUnitWithSomeLiterals:: Test
testClauseIsUnitWithSomeLiterals = 
    TestCase $ assertEqual "Should not return True for Clause.mk [some literal]" False (Clause.isUnit c)
                           where 
                               c = Clause.mk [Lit.mkNeg' 'a', Lit.mkPos' 'b']

---------------------------------------- IsMonotone TEST -------------------------------------------------------

testClauseIsMonotoneWithEmptyList :: Test
testClauseIsMonotoneWithEmptyList = 
    TestCase $ assertEqual "Should return True for Clause.mk []" True (Clause.isMonotone c)
                           where 
                               c = Clause.mk [] :: Clause.Clause Int

testClauseIsMonotoneWithPositiveLiteral :: Test
testClauseIsMonotoneWithPositiveLiteral = 
    TestCase $ assertEqual "Should return True for Clause.mk [some positive literal]" True (Clause.isMonotone c)
                           where 
                               c = Clause.mk [Lit.mkPos' "x1", Lit.mkPos' "x2", Lit.mkPos' "x3"]

testClauseIsMonotoneWithSomeLiteral :: Test
testClauseIsMonotoneWithSomeLiteral = 
    TestCase $ assertEqual "Should not return True for Clause.mk [some literal]" False (Clause.isMonotone c)
                           where 
                               c = Clause.mk [Lit.mkNeg' "x1", Lit.mkPos' "x2", Lit.mkNeg' "x3"]

testClauseIsMonotoneWithNegativeLiteral :: Test
testClauseIsMonotoneWithNegativeLiteral = 
    TestCase $ assertEqual "Should return True for Clause.mk [some negative literal]" True (Clause.isMonotone c)
                           where 
                               c = Clause.mk [Lit.mkNeg' "x1", Lit.mkNeg' "x2", Lit.mkNeg' "x3"]

---------------------------------------- isNegMonotone TEST -------------------------------------------------------

testClauseIsNegMonotoneWithEmptyList :: Test
testClauseIsNegMonotoneWithEmptyList = 
    TestCase $ assertEqual "Should return True for Clause.mk []" True (Clause.isNegMonotone c)
                           where 
                               c = Clause.mk [] :: Clause.Clause Int

testClauseIsNegMonotoneWithPositiveLiteral :: Test
testClauseIsNegMonotoneWithPositiveLiteral = 
    TestCase $ assertEqual "Should not return True for Clause.mk [some positive literal]" False (Clause.isNegMonotone c)
                           where 
                               c = Clause.mk [Lit.mkPos' "x1", Lit.mkPos' "x2", Lit.mkPos' "x3"]

testClauseIsNegMonotoneWithSomeLiteral :: Test
testClauseIsNegMonotoneWithSomeLiteral = 
    TestCase $ assertEqual "Should not return True for Clause.mk [some literal]" False (Clause.isNegMonotone c)
                           where 
                               c = Clause.mk [Lit.mkNeg' "x1", Lit.mkPos' "x2", Lit.mkNeg' "x3"]

testClauseIsNegMonotoneWithNegativeLiteral :: Test
testClauseIsNegMonotoneWithNegativeLiteral = 
    TestCase $ assertEqual "Should return True for Clause.mk [some negative literal]" True (Clause.isNegMonotone c)
                           where 
                               c = Clause.mk [Lit.mkNeg' "x1", Lit.mkNeg' "x2", Lit.mkNeg' "x3"]


---------------------------------------- isPosMonotone TEST -------------------------------------------------------

testClauseIsPosMonotoneWithEmptyList :: Test
testClauseIsPosMonotoneWithEmptyList = 
    TestCase $ assertEqual "Should return True for Clause.mk []" True (Clause.isPosMonotone c)
                           where 
                               c = Clause.mk [] :: Clause.Clause Int

testClauseIsPosMonotoneWithPositiveLiteral :: Test
testClauseIsPosMonotoneWithPositiveLiteral = 
    TestCase $ assertEqual "Should return True for Clause.mk [some positive literal]" True (Clause.isPosMonotone c)
                           where 
                               c = Clause.mk [Lit.mkPos' "x1", Lit.mkPos' "x2", Lit.mkPos' "x3"]

testClauseIsPosMonotoneWithSomeLiteral :: Test
testClauseIsPosMonotoneWithSomeLiteral = 
    TestCase $ assertEqual "Should not return True for Clause.mk [some literal]" False (Clause.isPosMonotone c)
                           where 
                               c = Clause.mk [Lit.mkNeg' "x1", Lit.mkPos' "x2", Lit.mkNeg' "x3"]

testClauseIsPosMonotoneWithNegativeLiteral :: Test
testClauseIsPosMonotoneWithNegativeLiteral = 
    TestCase $ assertEqual "Should not return True for Clause.mk [some negative literal]" False (Clause.isPosMonotone c)
                           where 
                               c = Clause.mk [Lit.mkNeg' "x1", Lit.mkNeg' "x2", Lit.mkNeg' "x3"]

--------------------------------------------- Size TEST -------------------------------------------------------
testClauseSizeWithEmptyList :: Test
testClauseSizeWithEmptyList = 
    TestCase $ assertEqual "Should return 0 for Clause.mk []" 0 (Clause.size c)
                           where 
                               c = Clause.mk [] :: Clause.Clause Int

testClauseSizeWithOneLiteral :: Test
testClauseSizeWithOneLiteral = 
    TestCase $ assertEqual "Should return 1 for Clause.mk [one literal]" 1 (Clause.size c)
                           where 
                               c = Clause.mk [Lit.mkPos' "x1"]

testClauseSizeWithTwoLiteralIdentical :: Test
testClauseSizeWithTwoLiteralIdentical = 
    TestCase $ assertEqual "Should return 1 for Clause.mk [one literal, one literal]" 1 (Clause.size c)
                           where 
                               c = Clause.mk [Lit.mkPos' "x1", Lit.mkPos' "x1"]

testClauseSizeWithBigList :: Test
testClauseSizeWithBigList = 
    TestCase $ assertEqual "Should return 100 for Clause.mk [100 différent literal]" 100 (Clause.size c)
                           where 
                               c = Clause.mk [Lit.mkPos' i | i <- [1..100]]

------------------------------------------ getVars TEST -------------------------------------------------------
testClauseGetVarsWithEmptyList :: Test
testClauseGetVarsWithEmptyList = 
    TestCase $ assertEqual "Should return [] for Clause.mk []" [] (Clause.getVars c)
                           where 
                               c = Clause.mk [] :: Clause.Clause Int

testClauseGetVarsWithOneLiteral :: Test
testClauseGetVarsWithOneLiteral = 
    TestCase $ assertEqual "Should return [x1] for Clause.mk [one literal]" [Var.mk "x1" ] (Clause.getVars c)
                           where 
                               c = Clause.mk [Lit.mkPos' "x1"]

testClauseGetVarsWithThreeLiteral :: Test
testClauseGetVarsWithThreeLiteral = 
    TestCase $ assertEqual "Should return [x1, x2, x3] for Clause.mk [three literal]" [Var.mk "x1" , Var.mk "x2" , Var.mk "x3"] (Clause.getVars c)
                           where 
                               c = Clause.mk [Lit.mkPos' "x1", Lit.mkNeg' "x2", Lit.mkPos' "x3"]

testClauseGetVarsWithTwoIdenticalLiteral :: Test
testClauseGetVarsWithTwoIdenticalLiteral = 
    TestCase $ assertEqual "Should return [x1, x2] for Clause.mk [three literal]" [Var.mk "x1", Var.mk "x2"] (Clause.getVars c)
                           where 
                               c = Clause.mk [Lit.mkPos' "x1", Lit.mkNeg' "x2", Lit.mkNeg' "x1"]                               

---------------------------------------------- MAIN -----------------------------------------------------------
main :: IO ()
main = do
    runTestTT $ TestList [testNewClauseIsEmpty, testClauseIsNotEmpty,                                                       -- isEmpty
                          testClauseIsUnitWithEmptyList, testClauseIsUnitWithOneLiteral, testClauseIsUnitWithSomeLiterals,  -- isUnit
                          testClauseIsMonotoneWithEmptyList, testClauseIsMonotoneWithPositiveLiteral,                       -- isMonotone
                          testClauseIsMonotoneWithSomeLiteral, testClauseIsMonotoneWithNegativeLiteral,
                          testClauseIsNegMonotoneWithEmptyList, testClauseIsNegMonotoneWithPositiveLiteral,                 -- isNegMonotone
                          testClauseIsNegMonotoneWithSomeLiteral, testClauseIsNegMonotoneWithNegativeLiteral,
                          testClauseIsPosMonotoneWithEmptyList, testClauseIsPosMonotoneWithPositiveLiteral,                 -- isPosMonotone
                          testClauseIsPosMonotoneWithSomeLiteral, testClauseIsPosMonotoneWithNegativeLiteral,
                          testClauseSizeWithEmptyList, testClauseSizeWithOneLiteral,                                        -- size
                          testClauseSizeWithTwoLiteralIdentical, testClauseSizeWithBigList,
                          testClauseGetVarsWithEmptyList, testClauseGetVarsWithOneLiteral,                                  -- getVars
                          testClauseGetVarsWithThreeLiteral, testClauseGetVarsWithTwoIdenticalLiteral
                          ]
    return()