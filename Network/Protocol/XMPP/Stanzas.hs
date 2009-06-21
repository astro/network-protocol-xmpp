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
	 Stanza(..)
	,GenericStanza(..)
	,Message(..)
	,Presence(..)
	) where

import Text.XML.HXT.DOM.Interface (XmlTree)
import Network.Protocol.XMPP.JID (JID, jidFormat)
import Network.Protocol.XMPP.Util (mkElement)
import qualified Text.XML.HXT.DOM.XmlNode as XN

class Stanza a where
	stanzaTo   :: a -> Maybe JID
	stanzaFrom :: a -> Maybe JID
	stanzaID   :: a -> String
	stanzaType :: a -> String
	stanzaLang :: a -> String
	stanzaXML  :: a -> XmlTree

data GenericStanza = GenericStanza
	{
		 genericStanzaTo   :: Maybe JID
		,genericStanzaFrom :: Maybe JID
		,genericStanzaID   :: String
		,genericStanzaType :: String
		,genericStanzaLang :: String
		,genericStanzaXML  :: XmlTree
	}

instance Stanza GenericStanza where
	stanzaTo   = genericStanzaTo
	stanzaFrom = genericStanzaFrom
	stanzaID   = genericStanzaID
	stanzaType = genericStanzaType
	stanzaLang = genericStanzaLang
	stanzaXML  = genericStanzaXML

data Message = Message
	{
		 messageTo   :: JID
		,messageFrom :: Maybe JID
		,messageID   :: String
		,messageType :: String
		,messageLang :: String
		,messageBody :: String
	}
	deriving (Show, Eq)

data Presence = Presence
	{
		 presenceTo     :: Maybe JID
		,presenceFrom   :: Maybe JID
		,presenceID     :: String
		,presenceType   :: String
		,presenceLang   :: String
		,presenceShow   :: String
		,presenceStatus :: String
	}
	deriving (Show, Eq)

instance Stanza Message where
	stanzaTo = Just . messageTo
	stanzaFrom = messageFrom
	stanzaID = messageID
	stanzaType = messageType
	stanzaLang = messageLang
	stanzaXML m = let
		attrs' = [
			 maybeAttr "to" $ (Just . jidFormat =<<) . stanzaTo
			,maybeAttr "from" $ (Just . jidFormat =<<) . stanzaFrom
			,mkAttr "id" $ stanzaID
			,mkAttr "type" $ stanzaType
		--	,mkAttr "lang" $ stanzaLang -- TODO: namespace support?
			]
		attrs = concat $ unmap m attrs'
		
		in mkElement ("jabber:client", "message")
			attrs
			[mkElement ("jabber:client", "body") []
				[XN.mkText $ messageBody m]]

instance Stanza Presence where
	stanzaTo = presenceTo
	stanzaFrom = presenceFrom
	stanzaID = presenceID
	stanzaType = presenceType
	stanzaLang = presenceLang
	stanzaXML p = let
		attrs' = [
			 maybeAttr "to" $ (Just . jidFormat =<<) . stanzaTo
			,maybeAttr "from" $ (Just . jidFormat =<<) . stanzaFrom
			,mkAttr "id" $ stanzaID
			,mkAttr "type" $ stanzaType
		--	,mkAttr "lang" $ stanzaLang -- TODO: namespace support?
			]
		attrs = concat $ unmap p attrs'
		
		showElem = case presenceShow p of
			"" -> []
			text -> [mkElement ("jabber:client", "show") []
				[XN.mkText text]]
		
		statusElem = case presenceStatus p of
			"" -> []
			text -> [mkElement ("jabber:client", "status") []
				[XN.mkText text]]
		
		in mkElement ("jabber:client", "presence")
			attrs
			(showElem ++ statusElem)

-------------------------------------------------------------------------------

unmap :: a -> [(a -> b)] -> [b]
unmap _ [] = []
unmap x (f:fs) = (f x):(unmap x fs)

maybeAttr :: (Stanza a) => String -> (a -> Maybe String) -> a -> [(String, String, String)]
maybeAttr attr f = mkAttr attr (\s -> maybe "" id (f s))

mkAttr :: (Stanza a) => String -> (a -> String) -> a -> [(String, String, String)]
mkAttr attr f stanza = case f stanza of
	"" -> []
	text -> [("", attr, text)]
