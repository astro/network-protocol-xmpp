{- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

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
	,formatJIDTests
	,jidPartTests
	]

buildNodeTests = "buildNodeTests" ~: TestList [
	-- TODO: stringprep, validation
	 testStr "" Nothing
	,testStr "a" (Just "a")
	]
	where testStr s expected = let
		maybeNode = mkJIDNode s
		value = maybe Nothing (\x -> Just (jidNodeStr x)) maybeNode
		in expected ~=? value

buildDomainTests = "buildDomainTests" ~: TestList [
	-- TODO: stringprep, validation
	 testStr "" Nothing
	,testStr "a" (Just "a")
	]
	where testStr s expected = let
		maybeDomain = mkJIDDomain s
		value = maybe Nothing (\x -> Just (jidDomainStr x)) maybeDomain
		in expected ~=? value

buildResourceTests = "buildResourceTests" ~: TestList [
	-- TODO: stringprep, validation
	 testStr "" Nothing
	,testStr "a" (Just "a")
	]
	where testStr s expected = let
		maybeResource = mkJIDResource s
		value = maybe Nothing (\x -> Just (jidResourceStr x)) maybeResource
		in expected ~=? value

buildJIDTests = "buildJIDTests" ~: TestList [
	-- TODO: stringprep, validation of segments
	 mkJID ""  ""  ""  ~?=  Nothing
	,mkJID "a" ""  ""  ~?=  Nothing
	,mkJID ""  "b" ""  ~?/= Nothing
	,mkJID ""  ""  "c" ~?=  Nothing
	,mkJID "a" "b" ""  ~?/= Nothing
	,mkJID "a" ""  "c" ~?=  Nothing
	,mkJID ""  "b" "c" ~?/= Nothing
	,mkJID "a" "b" "c" ~?/= Nothing
	]

parseJIDTests = "parseJIDTests" ~: TestList [
	 testJIDParse "b" (mkJID "" "b" "")
	,testJIDParse "a@b" (mkJID "a" "b" "")
	,testJIDParse "b/c" (mkJID "" "b" "c")
	,testJIDParse "a@b/c" (mkJID "a" "b" "c")
	]
	where testJIDParse s expected = expected ~=? (jidParse s)

formatJIDTests = "formatJIDTests" ~: TestList [
	 testJIDFormat (mkJID  "" "b"  "") "b"
	,testJIDFormat (mkJID "a" "b"  "") "a@b"
	,testJIDFormat (mkJID  "" "b" "c") "b/c"
	,testJIDFormat (mkJID "a" "b" "c") "a@b/c"
	]
	where testJIDFormat maybeJID expected = TestCase $ case maybeJID of
		Nothing -> assertFailure "mkJID returned Nothing"
		(Just jid) -> expected @=? (jidFormat jid)

jidPartTests = "jidPartTests" ~: TestList [
	 testJIDPart (mkJID  "" "b"  "") jidNode ""
	,testJIDPart (mkJID "a" "b"  "") jidNode "a"
	,testJIDPart (mkJID  "" "b"  "") jidDomain "b"
	,testJIDPart (mkJID  "" "b"  "") jidResource ""
	,testJIDPart (mkJID  "" "b" "c") jidResource "c"
	]
	where testJIDPart maybeJID f expected = TestCase $ case maybeJID of
		Nothing -> assertFailure "mkJID returned Nothing"
		(Just jid) -> expected @=? (f jid)

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
