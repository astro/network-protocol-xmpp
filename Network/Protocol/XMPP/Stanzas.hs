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

module Network.Protocol.XMPP.Stanzas (
	 StanzaType(..)
	,Stanza(..)
	,treeToStanza
	,stanzaToTree
	) where

import Text.XML.HXT.DOM.Interface (XmlTree)
import Text.XML.HXT.Arrow ((>>>), (&&&))
import qualified Text.XML.HXT.Arrow as A

import Network.Protocol.XMPP.JID (JID, jidFormat, jidParse)
import Network.Protocol.XMPP.Util (mkElement, mkQName)
import qualified Text.XML.HXT.DOM.XmlNode as XN

data StanzaType =
	  MessageNormal
	| MessageChat
	| MessageGroupChat
	| MessageHeadline
	| MessageError
	
	| PresenceUnavailable
	| PresenceSubscribe
	| PresenceSubscribed
	| PresenceUnsubscribe
	| PresenceUnsubscribed
	| PresenceProbe
	| PresenceError
	
	| IQGet
	| IQSet
	| IQResult
	| IQError
	deriving (Show, Eq)

data Stanza = Stanza
	{
		 stanzaType     :: StanzaType
		,stanzaTo       :: Maybe JID
		,stanzaFrom     :: Maybe JID
		,stanzaID       :: String
		,stanzaLang     :: String
		,stanzaPayloads :: [XmlTree]
	}
	deriving (Show, Eq)

stanzaTypeMap :: [((String, String, String), StanzaType)]
stanzaTypeMap = mkStanzaTypeMap $ [
	 ("jabber:client", "message", [
		 ("normal",    MessageNormal)
		,("chat",      MessageChat)
		,("groupchat", MessageGroupChat)
		,("headline",  MessageHeadline)
		,("error",     MessageError)
		])
	,("jabber:client", "presence", [
		 ("unavailable",  PresenceUnavailable)
		,("subscribe",    PresenceSubscribe)
		,("subscribed",   PresenceSubscribed)
		,("unsubscribe",  PresenceUnsubscribe)
		,("unsubscribed", PresenceUnsubscribed)
		,("probe",        PresenceProbe)
		,("error",        PresenceError)
		])
	,("jabber:client", "iq", [
		 ("get", IQGet)
		,("set", IQSet)
		,("result", IQResult)
		,("error", IQError)
		])
	]
	where mkStanzaTypeMap raw = do
		(ns, elementName, typeStrings) <- raw
		(typeString, type') <- typeStrings
		return ((ns, elementName, typeString), type')

stanzaTypeToStr :: StanzaType -> (String, String, String)
stanzaTypeToStr t = let
	step []              = undefined
	step ((ret, t'):tms)
		| t == t'    = ret
		| otherwise  = step tms
	in step stanzaTypeMap

stanzaTypeFromStr :: String -> String -> String -> Maybe StanzaType
stanzaTypeFromStr ns elementName typeString = let
	key = (ns, elementName, typeString)
	step [] = Nothing
	step ((key', ret):tms)
		| key == key' = Just ret
		| otherwise = step tms
	in step stanzaTypeMap

treeToStanza :: XmlTree -> [Stanza]
treeToStanza t = do
	to <- return . jidParse =<< A.runLA (A.getAttrValue "to") t
	from <- return . jidParse =<< A.runLA (A.getAttrValue "from") t
	id' <- A.runLA (A.getAttrValue "id") t
	lang <- A.runLA (A.getAttrValue "lang") t
	
	ns <- A.runLA A.getNamespaceUri t
	elementName <- A.runLA A.getLocalPart t
	typeString <- A.runLA (A.getAttrValue "type") t
	
	let payloads = A.runLA (A.getChildren >>> A.isElem) t
	
	case stanzaTypeFromStr ns elementName typeString of
		Nothing    -> []
		Just type' -> [Stanza type' to from id' lang payloads]

stanzaToTree :: Stanza -> XmlTree
stanzaToTree s = let
	(ns, elementName, typeString) = stanzaTypeToStr (stanzaType s)
	
	attrs' = [
		 autoAttr "to" (maybe "" jidFormat . stanzaTo)
		,autoAttr "from" (maybe "" jidFormat . stanzaFrom)
		,autoAttr "id" stanzaID
		,autoAttr "xml:lang" stanzaLang
		,\_ -> [("", "type", typeString)]
		]
	attrs = concatMap ($ s) attrs'
	in mkElement (ns, elementName) attrs (stanzaPayloads s)

autoAttr :: String -> (Stanza -> String) -> Stanza -> [(String, String, String)]
autoAttr attr f stanza = case f stanza of
	"" -> []
	text -> [("", attr, text)]
