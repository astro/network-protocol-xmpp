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
	,AuthenticatedClient
	,clientConnect
	,clientAuthenticate
	,clientBind
	,putTree
	,getTree
	) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), Handle)
import Codec.Binary.Base64.String (encode)
import Network (HostName, PortID, connectTo)
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.QualifiedName as QN

import Network.Protocol.XMPP.JID (JID)
import Network.Protocol.XMPP.SASL (Mechanism, bestMechanism)
import qualified Network.Protocol.XMPP.Stream as S
import Network.Protocol.XMPP.Stanzas (Stanza)
import Network.Protocol.XMPP.Util (mkElement)

data ConnectedClient = ConnectedClient JID S.Stream Handle

data AuthenticatedClient = AuthenticatedClient JID JID S.Stream Handle

type Username = String
type Password = String

clientConnect :: JID -> HostName -> PortID -> IO ConnectedClient
clientConnect jid host port = do
	handle <- connectTo host port
	hSetBuffering handle NoBuffering
	
	stream <- S.beginStream jid handle
	
	-- TODO: TLS support
	
	return $ ConnectedClient jid stream handle

clientAuthenticate :: ConnectedClient -> JID -> Username -> Password -> IO AuthenticatedClient
clientAuthenticate (ConnectedClient serverJID stream h) jid username password = let
	mechanisms = (advertisedMechanisms . S.streamFeatures) stream
	saslMechanism = case bestMechanism mechanisms of
		Nothing -> error "No supported SASL mechanism"
		Just m -> m
	in do
		-- TODO: use detected mechanism
		
		let saslText = concat [(show jid), "\x00", username, "\x00", password]
		let b64Text = encode saslText
		
		S.putTree stream $ mkElement ("", "auth")
			[ ("", "xmlns", "urn:ietf:params:xml:ns:xmpp-sasl")
			 ,("", "mechanism", "PLAIN")]
			[XN.mkText b64Text]
		
		response <- S.getTree stream
		
		-- TODO: check if response is success or failure
		
		newStream <- S.beginStream serverJID h
		return $ AuthenticatedClient serverJID jid newStream h

clientBind :: AuthenticatedClient -> IO ()
clientBind c@(AuthenticatedClient _ _ stream h) = do
	-- Bind
	-- TODO: request specific resource
	-- TODO: set ID to random value, and check bind result for JID
	-- TODO: return JID from server
	putTree c $ mkElement ("", "iq")
		[("", "type", "set")]
		[ mkElement ("", "bind")
		  	[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-bind")]
		  	[]]
	
	bindResult <- getTree c
	
	-- Session
	putTree c $ mkElement ("", "iq")
		[("", "type", "set")]
		[mkElement ("", "session")
			[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-session")]
			[]]
	
	sessionResult <- getTree c
	
	putTree c $ mkElement ("", "presence") [] []
	getTree c
	return ()

advertisedMechanisms :: [S.StreamFeature] -> [Mechanism]
advertisedMechanisms [] = []
advertisedMechanisms (f:fs) = case f of
	(S.FeatureSASL ms) -> ms
	otherwise -> advertisedMechanisms fs

-------------------------------------------------------------------------------

putTree :: AuthenticatedClient -> XmlTree -> IO ()
putTree (AuthenticatedClient _ _ s _) = S.putTree s

getTree :: AuthenticatedClient -> IO XmlTree
getTree (AuthenticatedClient _ _ s _) = S.getTree s

