module Main () where

import Test.HUnit
import Tests.Core (coreTests)

allTests = "allTests" ~: TestList [coreTests]

main = do
	runTestTT allTests
