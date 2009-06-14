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
import Network.Protocol.XMPP.JID (JID)
import Network.Protocol.XMPP.Stream (beginStream, streamFeatures)
import Network.Protocol.XMPP.Stanzas (Stanza)

data ConnectedClient = ConnectedClient JID Handle

data AuthenticatedClient = AuthenticatedClient Handle HostName PortID

type Username = String
type Password = String

clientConnect :: JID -> HostName -> PortID -> IO ConnectedClient
clientConnect jid host port = do
	handle <- connectTo host port
	stream <- beginStream jid host handle
	putStrLn $ "streamFeatures = " ++ (show (streamFeatures stream))
	return $ ConnectedClient jid handle

clientAuthenticate :: ConnectedClient -> Username -> Password -> AuthenticatedClient
clientAuthenticate = undefined

clientSend :: (Stanza s) => AuthenticatedClient -> s -> IO ()
clientSend = undefined

