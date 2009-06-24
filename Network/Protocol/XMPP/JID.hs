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

module Network.Protocol.XMPP.JID (
	 JID(..)
	,JIDNode
	,JIDDomain
	,JIDResource
	
	,jidNodeStr
	,jidDomainStr
	,jidResourceStr
	
	,mkJIDNode
	,mkJIDDomain
	,mkJIDResource
	,mkJID
	
	,jidNode
	,jidDomain
	,jidResource
	
	,jidParse
	,jidFormat
	) where

data JID = JID (Maybe JIDNode) JIDDomain (Maybe JIDResource)
	deriving (Eq, Show)

newtype JIDNode = JIDNode String
	deriving (Eq, Show)
	
newtype JIDDomain = JIDDomain String
	deriving (Eq, Show)
	
newtype JIDResource = JIDResource String
	deriving (Eq, Show)

jidNodeStr :: JIDNode -> String
jidNodeStr (JIDNode s) = s

jidDomainStr :: JIDDomain -> String
jidDomainStr (JIDDomain s) = s

jidResourceStr :: JIDResource -> String
jidResourceStr (JIDResource s) = s

mkJIDNode :: String -> Maybe JIDNode
mkJIDNode "" = Nothing
mkJIDNode s = Just (JIDNode s) -- TODO: stringprep, validation

mkJIDDomain :: String -> Maybe JIDDomain
mkJIDDomain "" = Nothing
mkJIDDomain s = Just (JIDDomain s) -- TODO: stringprep, validation

mkJIDResource :: String -> Maybe JIDResource
mkJIDResource "" = Nothing
mkJIDResource s = Just (JIDResource s) -- TODO: stringprep, validation

mkJID :: String -> String -> String -> Maybe JID
mkJID nodeStr domainStr resourceStr = let
	node = mkJIDNode nodeStr
	resource = mkJIDResource resourceStr
	in do
		domain <- mkJIDDomain domainStr
		Just (JID node domain resource)

jidNode :: JID -> String
jidNode (JID x _ _) = maybe "" jidNodeStr x

jidDomain :: JID -> String
jidDomain (JID _ x _) = jidDomainStr x

jidResource :: JID -> String
jidResource (JID _ _ x) = maybe "" jidResourceStr x

-- TODO: validate input according to RFC 3920, section 3.1
jidParse :: String -> Maybe JID
jidParse s = let
	(nodeStr, postNode) = if '@' `elem` s then split s '@' else ("", s)
	(domainStr, resourceStr) = if '/' `elem` postNode then split postNode '/' else (postNode, "")
	in mkJID nodeStr domainStr resourceStr

jidFormat :: JID -> String
jidFormat (JID node (JIDDomain domain) resource) = let
	nodeStr = maybe "" (\(JIDNode s) -> s ++ "@") node
	resourceStr = maybe "" (\(JIDResource s) -> "/" ++ s) resource
	in concat [nodeStr, domain, resourceStr]

split :: (Eq a) => [a] -> a -> ([a], [a])
split xs final = let
	(before, rawAfter) = span (/= final) xs
	after = safeTail rawAfter
	in (before, after)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs
