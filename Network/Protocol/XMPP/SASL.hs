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

module Network.Protocol.XMPP.SASL (
	 Result(..)
	,authenticate
	) where

import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Network.Protocol.SASL.GSASL as G

import Network.Protocol.XMPP.JID (JID, jidFormat)
import Network.Protocol.XMPP.Util (mkElement, mkQName)
import qualified Network.Protocol.XMPP.Stream as S

type Username = String
type Password = String
type Mechanism = String

data Result = Success | Failure
	deriving (Show, Eq)

authenticate :: S.Stream -> JID -> Username -> Password -> IO Result
authenticate stream jid username password = do
	let mechanisms = (advertisedMechanisms . S.streamFeatures) stream
	let authz = jidFormat jid
	
	ctxt <- G.mkContext
	G.propertySet s G.GSASL_AUTHZID (jidFormat jid)
	G.propertySet s G.GSASL_AUTHID username
	G.propertySet s G.GSASL_PASSWORD password
	
	-- TODO: use best mechanism
	s <- G.clientStart ctxt "PLAIN"
	(b64text, rc) <- G.step64 s ""
	
	S.putTree stream $ mkElement ("", "auth")
		[ ("", "xmlns", "urn:ietf:params:xml:ns:xmpp-sasl")
		 ,("", "mechanism", "PLAIN")]
		[XN.mkText b64text]
	
	successElem <- A.runX (
		A.arrIO (\_ -> S.getTree stream)
		A.>>> A.getChildren
		A.>>> A.hasQName (mkQName "urn:ietf:params:xml:ns:xmpp-sasl" "success"))
	
	if length successElem == 0
		then return Failure
		else return Success

advertisedMechanisms :: [S.StreamFeature] -> [Mechanism]
advertisedMechanisms [] = []
advertisedMechanisms (f:fs) = case f of
	(S.FeatureSASL ms) -> ms
	_ -> advertisedMechanisms fs

