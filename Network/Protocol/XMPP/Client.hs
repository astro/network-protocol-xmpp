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

module Network.Protocol.XMPP.Client (
	 ConnectedClient
	,Client
	,clientConnect
	,clientAuthenticate
	,clientBind
	,clientJID
	,clientServerJID
	,putTree
	,getTree
	,putStanza
	) where

import Codec.Binary.Base64.String (encode)
import Network (HostName, PortID, connectTo)
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import qualified Text.XML.HXT.DOM.XmlNode as XN

import Network.Protocol.XMPP.JID (JID, jidParse, jidFormat, jidResource)
import qualified Network.Protocol.XMPP.SASL as SASL
import qualified Network.Protocol.XMPP.Stream as S
import Network.Protocol.XMPP.Util (mkElement, mkQName)
import Network.Protocol.XMPP.Stanzas (Stanza, stanzaXML)

data ConnectedClient = ConnectedClient JID S.Stream

data Client = Client {
	 clientJID        :: JID
	,clientServerJID  :: JID
	,clientStream     :: S.Stream
	}

type Username = String
type Password = String

clientConnect :: JID -> HostName -> PortID -> IO ConnectedClient
clientConnect jid host port = do
	handle <- connectTo host port
	stream <- S.beginStream jid handle
	return $ ConnectedClient jid stream

clientAuthenticate :: ConnectedClient -> JID -> Username -> Password -> IO Client
clientAuthenticate (ConnectedClient serverJID stream) jid username password = do
	authed <- SASL.authenticate stream jid serverJID username password
	case authed of
		SASL.Failure -> error "Authentication failure"
		_ -> do
			newStream <- S.restartStream stream
			return $ Client jid serverJID newStream

clientBind :: Client -> IO JID
clientBind c = do
	-- Bind
	-- TODO: set ID to random value, and check bind result for JID
	let resourceElements = case jidResource . clientJID $ c of
		"" -> []
		resource ->
			[mkElement ("", "resource")
				[]
				[XN.mkText resource]]
	
	putTree c $ mkElement ("", "iq")
		[("", "type", "set")]
		[mkElement ("", "bind")
		 	[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-bind")]
		 	resourceElements]
	
	bindResult <- getTree c
	let [rawJID] = A.runLA (
		A.deep (A.hasQName (mkQName "urn:ietf:params:xml:ns:xmpp-bind" "jid"))
		>>> A.getChildren
		>>> A.getText) bindResult
	let jid = case jidParse rawJID of
		Just x -> x
		_ -> error "Couldn't parse server's returned JID"
	
	-- Session
	putTree c $ mkElement ("", "iq")
		[("", "type", "set")]
		[mkElement ("", "session")
			[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-session")]
			[]]
	
	getTree c
	
	putTree c $ mkElement ("", "presence") [] []
	getTree c
	return jid

-------------------------------------------------------------------------------

putTree :: Client -> XmlTree -> IO ()
putTree = S.putTree . clientStream

getTree :: Client -> IO XmlTree
getTree = S.getTree . clientStream

putStanza :: (Stanza a) => Client -> a -> IO ()
putStanza c = (putTree c) . stanzaXML
