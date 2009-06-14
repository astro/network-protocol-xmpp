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

module Network.Protocol.XMPP.Stream (
	 Stream (
	 	 streamHostName
	 	,streamLanguage
	 	,streamVersion
	 	,streamFeatures
	 	)
	,beginStream
	,send
	) where

import qualified System.IO as IO
import Network (HostName, PortID, connectTo)
import qualified Network.Protocol.XMPP.IncrementalXML as XML
import Data.AssocList (lookupDef)
import qualified Text.XML.HXT.DOM.QualifiedName as QN
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.DOM.FormatXmlTree (formatXmlTree)
import Text.XML.HXT.DOM.Util (attrEscapeXml)
import Text.XML.HXT.Arrow ((>>>), (>>.))
import Data.Tree.NTree.TypeDefs (NTree(NTree))
import qualified Text.XML.HXT.Arrow as A
import Network.Protocol.XMPP.JID (JID)
import Network.Protocol.XMPP.Stanzas (Stanza)
import Network.Protocol.XMPP.XMLBuilder (eventsToTree)

maxXMPPVersion = XMPPVersion 1 0

data Stream = Stream
	{
		 streamHandle :: IO.Handle
		,streamParser :: XML.Parser
		,streamHostName :: HostName
		,streamLanguage :: XMLLanguage
		,streamVersion :: XMPPVersion
		,streamFeatures :: [StreamFeature]
	}

data StreamFeature =
	  FeatureStartTLS Bool
	| FeatureSASL [SASLMechanism]
	| FeatureRegister
	| FeatureUnknown XmlTree
	| FeatureDebug String
	deriving (Show, Eq)

newtype XMLLanguage = XMLLanguage String
	deriving (Show, Eq)

newtype SASLMechanism = SASLMechanism String
	deriving (Show, Eq)

data XMPPVersion = XMPPVersion Int Int
	deriving (Show, Eq)

-------------------------------------------------------------------------------

beginStream :: JID -> HostName -> IO.Handle -> IO Stream
beginStream jid host handle = do
	parser <- XML.newParser
	
	IO.hSetBuffering handle IO.NoBuffering
	
	-- Since only the opening tag should be written, normal XML
	-- serialization cannot be used. Be careful to escape any embedded
	-- attributes.
	IO.hPutStr handle $
		"<?xml version='1.0'?>\n" ++
		"<stream:stream xmlns='jabber:client'" ++
		" to='" ++ (attrEscapeXml . show) jid ++ "'" ++
		" version='1.0'" ++
		" xmlns:stream='http://etherx.jabber.org/streams'>"
	
	IO.hFlush handle
	
	xmlChars <- hGetChars handle 100
	events <- (XML.incrementalParse parser xmlChars)
	return $ beginStream' handle parser events

beginStream' handle parser (streamStart:events) = let
	-- TODO: parse from streamStart
	host = "localhost"
	language = XMLLanguage "en"
	version = XMPPVersion 1 0
	
	featuresName = QN.mkNsName "features" "http://etherx.jabber.org/streams"
	
	eventTree = eventsToTree events
	featureRoots = A.runLA (
		A.getChildren
		>>> A.hasQName featuresName) eventTree
	features = case featureRoots of
		[] -> []
		(t:_) -> map parseFeature (A.runLA A.getChildren t)
	
	in Stream handle parser host language version features

parseFeature :: XmlTree -> StreamFeature
parseFeature t = lookupDef FeatureUnknown qname [
	 (("urn:ietf:params:xml:ns:xmpp-tls", "starttls"), parseFeatureTLS)
	,(("urn:ietf:params:xml:ns:xmpp-sasl", "mechanisms"), parseFeatureSASL)
	,(("http://jabber.org/features/iq-register", "register"), (\_ -> FeatureRegister))
	] t
	where
		qname = maybe ("", "") (\n -> (QN.namespaceUri n, QN.localPart n)) (XN.getName t)

parseFeatureTLS :: XmlTree -> StreamFeature
parseFeatureTLS t = FeatureStartTLS True -- TODO: detect whether or not required

parseFeatureSASL :: XmlTree -> StreamFeature
parseFeatureSASL t = let
	mechName = QN.mkNsName "mechanism" "urn:ietf:params:xml:ns:xmpp-sasl"
	rawMechanisms = A.runLA (
		A.getChildren
		>>> A.hasQName mechName
		>>> A.getChildren
		>>> A.getText) t
	
	-- TODO: validate mechanism names according to SASL rules
	-- <20 chars, uppercase, alphanum, etc
	in FeatureSASL [SASLMechanism n | n <- rawMechanisms]

-------------------------------------------------------------------------------

send :: (Stanza s) => Stream -> s -> IO ()
send = undefined

-------------------------------------------------------------------------------

hGetChars :: IO.Handle -> Int -> IO String
hGetChars h timeout = do
	have_input <- IO.hWaitForInput h timeout
	case have_input of
		False -> return []
		True -> do
			chr <- IO.hGetChar h
			next <- hGetChars h timeout
			return $ chr : next

