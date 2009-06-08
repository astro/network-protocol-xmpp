module Tests.Core (coreTests) where

import Control.Monad (unless)
import Test.HUnit
import Network.Protocol.XMPP

coreTests = "coreTests" ~: TestList [jidTests]

-------------------------------------------------------------------------------

jidTests = "jidTests" ~: TestList [
	 buildNodeTests
	,buildDomainTests
	,buildResourceTests
	,buildJIDTests
	,parseJIDTests
	,showJIDTests
	]

buildNodeTests = "buildNodeTests" ~: TestList [
	-- TODO: stringprep, validation
	 testValue "" Nothing
	,testValue "a" (Just "a")
	]
	where testValue s expected = let
		maybeNode = jidNodeBuild s
		value = maybe Nothing (\x -> Just (jidNodeValue x)) maybeNode
		in expected ~=? value

buildDomainTests = "buildDomainTests" ~: TestList [
	-- TODO: stringprep, validation
	 testValue "" Nothing
	,testValue "a" (Just "a")
	]
	where testValue s expected = let
		maybeDomain = jidDomainBuild s
		value = maybe Nothing (\x -> Just (jidDomainValue x)) maybeDomain
		in expected ~=? value

buildResourceTests = "buildResourceTests" ~: TestList [
	-- TODO: stringprep, validation
	 testValue "" Nothing
	,testValue "a" (Just "a")
	]
	where testValue s expected = let
		maybeResource = jidResourceBuild s
		value = maybe Nothing (\x -> Just (jidResourceValue x)) maybeResource
		in expected ~=? value

buildJIDTests = "buildJIDTests" ~: TestList [
	-- TODO: stringprep, validation of segments
	 jidBuild ""  ""  ""  ~?=  Nothing
	,jidBuild "a" ""  ""  ~?=  Nothing
	,jidBuild ""  "b" ""  ~?/= Nothing
	,jidBuild ""  ""  "c" ~?=  Nothing
	,jidBuild "a" "b" ""  ~?/= Nothing
	,jidBuild "a" ""  "c" ~?=  Nothing
	,jidBuild ""  "b" "c" ~?/= Nothing
	,jidBuild "a" "b" "c" ~?/= Nothing
	]

parseJIDTests = "parseJIDTests" ~: TestList [
	 testJIDParse "b" (jidBuild "" "b" "")
	,testJIDParse "a@b" (jidBuild "a" "b" "")
	,testJIDParse "b/c" (jidBuild "" "b" "c")
	,testJIDParse "a@b/c" (jidBuild "a" "b" "c")
	]
	where testJIDParse s expected = expected ~=? (jidParse s)

showJIDTests = "showJIDTests" ~: TestList [
	 testJIDShow (jidBuild "" "b" "") "b"
	,testJIDShow (jidBuild "a" "b" "") "a@b"
	,testJIDShow (jidBuild "" "b" "c") "b/c"
	,testJIDShow (jidBuild "a" "b" "c") "a@b/c"
	]
	where testJIDShow maybeJID expected = TestCase (case maybeJID of
		Nothing -> assertFailure "jidBuild returned Nothing"
		(Just jid) -> expected @=? (show jid))

-------------------------------------------------------------------------------

assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual preface unexpected actual =
	unless (actual /= unexpected) (assertFailure msg)
	where msg = (if null preface then "" else preface ++ "\n") ++
	            "got unexpected: " ++ show actual

(@/=?) :: (Eq a, Show a) => a -> a -> Assertion
unexpected @/=? actual = assertNotEqual "" unexpected actual

(@?/=) :: (Eq a, Show a) => a -> a -> Assertion
actual @?/= unexpected = assertNotEqual "" unexpected actual

(~/=?) :: (Eq a, Show a) => a -> a -> Test
unexpected ~/=? actual = TestCase (unexpected @/=? actual)

(~?/=) :: (Eq a, Show a) => a -> a -> Test
actual ~?/= unexpected = TestCase (actual @?/= unexpected)
