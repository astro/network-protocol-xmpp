{- Copyright (C) 2010 Stephan Maka <stephan@spaceboyz.net>
   
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


module Network.Protocol.XMPP.Component (
	 ConnectedComponent
	,Component
	,componentConnect
	,componentAuthenticate
	,componentJID
	) where

import Control.Monad (when)
import Network (HostName, PortID, connectTo)
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Data.Digest.Pure.SHA as SHA

import Network.Protocol.XMPP.JID (JID, jidParse, jidResource)
import qualified Network.Protocol.XMPP.SASL as SASL
import qualified Network.Protocol.XMPP.Stream as S
import Network.Protocol.XMPP.Util (mkElement, mkQName)
import Network.Protocol.XMPP.Stanzas (Stanza, stanzaToTree)
import Network.Protocol.XMPP.Connection
import qualified Data.ByteString.Lazy.Char8 as B (pack)

data ConnectedComponent = ConnectedComponent JID S.Stream

data Component = Component {
	 componentJID :: JID
	,componentStream :: S.Stream
	}

type Password = String

componentConnect :: JID -> HostName -> PortID -> IO ConnectedComponent
componentConnect jid host port = do
	handle <- connectTo host port
	stream <- S.beginStream jid "jabber:component:accept" handle
	return $ ConnectedComponent jid stream

componentAuthenticate :: ConnectedComponent -> Password -> IO Component
componentAuthenticate (ConnectedComponent jid stream) password
    = do let c = Component jid stream

         let S.XMPPStreamID sid = S.streamID stream
             hash = SHA.showDigest . SHA.sha1 . B.pack $ sid ++ password
         putTree c $ mkElement ("", "handshake") [] [XN.mkText hash]

         result <- getTree c
         when (A.runLA (A.getChildren
                        >>> A.hasQName (mkQName "jabber:component:accept" "handshake")
                       ) result == []) $
             error "Component handshake failed"

         return c

-------------------------------------------------------------------------------

instance Connection Component where
    getTree = S.getTree . componentStream
    putTree = S.putTree . componentStream
