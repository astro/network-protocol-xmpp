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
	 JID
	,JIDNode
	,JIDDomain
	,JIDResource
	
	,jidNodeBuild
	,jidNodeValue
	,jidDomainBuild
	,jidDomainValue
	,jidResourceBuild
	,jidResourceValue
	,jidBuild
	
	,jidParse
	,jidFormat
	) where

data JID = JID (Maybe JIDNode) JIDDomain (Maybe JIDResource)
	deriving (Eq)

instance Show JID where
	show = jidFormat

newtype JIDNode = JIDNode String
	deriving (Eq, Show)
	
newtype JIDDomain = JIDDomain String
	deriving (Eq, Show)
	
newtype JIDResource = JIDResource String
	deriving (Eq, Show)

jidNodeBuild :: String -> Maybe JIDNode
jidNodeBuild "" = Nothing
jidNodeBuild s = Just (JIDNode s) -- TODO: stringprep, validation

jidNodeValue :: JIDNode -> String
jidNodeValue (JIDNode s) = s

jidDomainBuild :: String -> Maybe JIDDomain
jidDomainBuild "" = Nothing
jidDomainBuild s = Just (JIDDomain s) -- TODO: stringprep, validation

jidDomainValue :: JIDDomain -> String
jidDomainValue (JIDDomain s) = s

jidResourceBuild :: String -> Maybe JIDResource
jidResourceBuild "" = Nothing
jidResourceBuild s = Just (JIDResource s) -- TODO: stringprep, validation

jidResourceValue :: JIDResource -> String
jidResourceValue (JIDResource s) = s

jidBuild :: String -> String -> String -> Maybe JID
jidBuild nodeStr domainStr resourceStr = let
	node = jidNodeBuild nodeStr
	resource = jidResourceBuild resourceStr
	in do
		domain <- jidDomainBuild domainStr
		Just (JID node domain resource)

-- TODO: validate input according to RFC 3920, section 3.1
jidParse :: String -> Maybe JID
jidParse s = let
	(nodeStr, postNode) = if '@' `elem` s then split s '@' else ("", s)
	(domainStr, resourceStr) = if '/' `elem` postNode then split postNode '/' else (postNode, "")
	in jidBuild nodeStr domainStr resourceStr

jidFormat :: JID -> String
jidFormat (JID node (JIDDomain domain) resource) = let
	nodeStr = maybe "" (\(JIDNode s) -> s ++ "@") node
	resourceStr = maybe "" (\(JIDResource s) -> "/" ++ s) resource
	in concat [nodeStr, domain, resourceStr]

split xs final = let
	(before, rawAfter) = span (/= final) xs
	after = case rawAfter of
		[] -> []
		xs -> tail xs
	in (before, after)
