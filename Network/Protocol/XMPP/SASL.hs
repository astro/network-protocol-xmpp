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

import Text.XML.HXT.Arrow ((>>>))
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

authenticate :: S.Stream -> JID -> JID -> Username -> Password -> IO Result
authenticate stream userJID serverJID username password = do
	let mechanisms = (advertisedMechanisms . S.streamFeatures) stream
	let authz = jidFormat userJID
	let hostname = jidFormat serverJID
	
	G.withContext $ \ctxt -> do
	
	suggested <- G.clientSuggestMechanism ctxt mechanisms
	mechanism <- case suggested of
		Just m -> return m
		Nothing -> error "No supported SASL mechanisms advertised"
	
	G.withSession (G.clientStart ctxt mechanism) $ \s -> do
	
	G.propertySet s G.GSASL_AUTHZID authz
	G.propertySet s G.GSASL_AUTHID username
	G.propertySet s G.GSASL_PASSWORD password
	G.propertySet s G.GSASL_SERVICE "xmpp"
	G.propertySet s G.GSASL_HOSTNAME hostname
	
	(b64text, rc) <- G.step64 s ""
	S.putTree stream $ mkElement ("", "auth")
		[ ("", "xmlns", "urn:ietf:params:xml:ns:xmpp-sasl")
		 ,("", "mechanism", mechanism)]
		[XN.mkText b64text]
	
	case rc of
		G.GSASL_OK -> saslFinish stream
		G.GSASL_NEEDS_MORE -> saslLoop stream s

saslLoop :: S.Stream -> G.Session -> IO Result
saslLoop stream session = do
	challengeText <- A.runX (
		A.arrIO (\_ -> S.getTree stream)
		>>> A.getChildren
		>>> A.hasQName (mkQName "urn:ietf:params:xml:ns:xmpp-sasl" "challenge")
		>>> A.getChildren >>> A.getText)
	
	if null challengeText then return Failure
		else do
			(b64text, rc) <- G.step64 session (concat challengeText)
			S.putTree stream $ mkElement ("", "response")
				[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-sasl")]
				[XN.mkText b64text]
			case rc of
				G.GSASL_OK -> saslFinish stream
				G.GSASL_NEEDS_MORE -> saslLoop stream session

saslFinish :: S.Stream -> IO Result
saslFinish stream = do
	successElem <- A.runX (
		A.arrIO (\_ -> S.getTree stream)
		>>> A.getChildren
		>>> A.hasQName (mkQName "urn:ietf:params:xml:ns:xmpp-sasl" "success"))
	
	return $ if null successElem then Failure else Success

advertisedMechanisms :: [S.StreamFeature] -> [Mechanism]
advertisedMechanisms [] = []
advertisedMechanisms (f:fs) = case f of
	(S.FeatureSASL ms) -> ms
	_ -> advertisedMechanisms fs

