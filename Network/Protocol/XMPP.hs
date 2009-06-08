module Network.Protocol.XMPP (
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

-------------------------------------------------------------------------------

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
jidBuild nodeStr domainStr resourceStr = case (jidDomainBuild domainStr) of
	Nothing -> Nothing
	(Just domain) -> Just (JID node domain resource)
	where
		node = jidNodeBuild nodeStr
		resource = jidResourceBuild resourceStr

-- TODO: validate input according to RFC 3920, section 3.1
jidParse :: String -> Maybe JID
jidParse s = jidBuild nodeStr domainStr resourceStr
	where
		(nodeStr, postNode) = if '@' `elem` s then split s '@' else ("", s)
		(domainStr, resourceStr) = if '/' `elem` postNode then split postNode '/' else (postNode, "")
		
jidFormat :: JID -> String
jidFormat (JID node (JIDDomain domain) resource) = concat [nodeStr, domain, resourceStr]
	where
		nodeStr = case node of
			Nothing -> ""
			Just (JIDNode s) -> s ++ "@"
		resourceStr = case resource of
			Nothing -> ""
			Just (JIDResource s) -> "/" ++ s

-------------------------------------------------------------------------------

split xs final = (before, after)
	where
		(before, rawAfter) = span (/= final) xs
		after = case rawAfter of
			[] -> []
			xs -> tail xs
