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
	,clientSend
	) where

import System.IO (Handle)
import Network (HostName, PortID, connectTo)
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.QualifiedName as QN

import Network.Protocol.XMPP.JID (JID)
import Network.Protocol.XMPP.SASL (Mechanism, bestMechanism)
import qualified Network.Protocol.XMPP.Stream as S
import Network.Protocol.XMPP.Stanzas (Stanza)

data ConnectedClient = ConnectedClient JID S.Stream

data AuthenticatedClient = AuthenticatedClient JID S.Stream

type Username = String
type Password = String

clientConnect :: JID -> HostName -> PortID -> IO ConnectedClient
clientConnect jid host port = do
	handle <- connectTo host port
	stream <- S.beginStream jid host handle
	
	-- TODO: TLS support
	
	return $ ConnectedClient jid stream

clientAuthenticate :: ConnectedClient -> Username -> Password -> IO AuthenticatedClient
clientAuthenticate (ConnectedClient jid stream) username password = let
	mechanisms = (advertisedMechanisms . S.streamFeatures) stream
	saslMechanism = case bestMechanism mechanisms of
		Nothing -> error "No supported SASL mechanism"
		Just m -> m
	in do
		putStrLn $ "mechanism = " ++ (show saslMechanism)
		
		-- TODO: use detected mechanism
		S.putTree stream $ XN.mkElement
			(QN.mkName "auth")
			[
				 XN.mkAttr (QN.mkName "xmlns") [XN.mkText "urn:ietf:params:xml:ns:xmpp-sasl"]
				,XN.mkAttr (QN.mkName "mechanism") [XN.mkText "PLAIN"]
			]
			[XN.mkText "="]
		
		response <- S.getTree stream
		putStrLn $ "response:"
		A.runX (A.constA response >>> A.putXmlTree "-")
		
		return $ AuthenticatedClient jid stream

clientSend :: (Stanza s) => AuthenticatedClient -> s -> IO ()
clientSend = undefined

advertisedMechanisms :: [S.StreamFeature] -> [Mechanism]
advertisedMechanisms [] = []
advertisedMechanisms (f:fs) = case f of
	(S.FeatureSASL ms) -> ms
	otherwise -> advertisedMechanisms fs

