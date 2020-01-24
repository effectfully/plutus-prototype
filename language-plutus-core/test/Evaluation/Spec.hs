module Evaluation.Spec where

import           Evaluation.Constant        (test_constant)
import           Evaluation.DynamicBuiltins (test_dynamicBuiltins)
import           Evaluation.Golden          (test_golden)
import           Evaluation.Machines        (test_budget, test_machines, test_memory, test_counting)

import           Test.Tasty

test_evaluation :: TestTree
test_evaluation =
    testGroup "evaluation"
        [ test_constant
        , test_dynamicBuiltins
        , test_golden
        , test_machines
        , test_memory
        , test_budget
        , test_counting
        ]
