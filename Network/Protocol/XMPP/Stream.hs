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
	 	 streamLanguage
	 	,streamVersion
	 	,streamFeatures
	 	)
	,StreamFeature (
		 FeatureStartTLS
		,FeatureSASL
		,FeatureRegister
		)
	,beginStream
	,getTree
	,putTree
	) where

import qualified System.IO as IO
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
import Network.Protocol.XMPP.SASL (Mechanism, findMechanism)
import Network.Protocol.XMPP.XMLBuilder (eventsToTree)

maxXMPPVersion = XMPPVersion 1 0

data Stream = Stream
	{
		 streamHandle :: IO.Handle
		,streamParser :: XML.Parser
		,streamLanguage :: XMLLanguage
		,streamVersion :: XMPPVersion
		,streamFeatures :: [StreamFeature]
	}

data StreamFeature =
	  FeatureStartTLS Bool
	| FeatureSASL [Mechanism]
	| FeatureRegister
	| FeatureUnknown XmlTree
	| FeatureDebug String
	deriving (Show, Eq)

newtype XMLLanguage = XMLLanguage String
	deriving (Show, Eq)

data XMPPVersion = XMPPVersion Int Int
	deriving (Show, Eq)

-------------------------------------------------------------------------------

beginStream :: JID -> IO.Handle -> IO Stream
beginStream jid handle = do
	parser <- XML.newParser
	
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
	
	[startStreamEvent] <- readEventsUntil startOfStream handle parser 1000
	featureTree <- getTree' handle parser
	return $ beginStream' handle parser startStreamEvent featureTree
	where
		streamName = QN.mkNsName "stream" "http://etherx.jabber.org/streams"
		startOfStream depth event = case (depth, event) of
			(1, (XML.BeginElement streamName _)) -> True
			otherwise -> False

beginStream' handle parser streamStart featureTree = let
	-- TODO: parse from streamStart
	language = XMLLanguage "en"
	version = XMPPVersion 1 0
	
	featuresName = QN.mkNsName "features" "http://etherx.jabber.org/streams"
	
	featureRoots = A.runLA (
		A.getChildren
		>>> A.hasQName featuresName) featureTree
	features = case featureRoots of
		[] -> []
		(t:_) -> map parseFeature (A.runLA A.getChildren t)
	
	in Stream handle parser language version features

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
	in FeatureSASL (map findMechanism rawMechanisms)

-------------------------------------------------------------------------------

getTree :: Stream -> IO XmlTree
getTree s = getTree' (streamHandle s) (streamParser s)

getTree' :: IO.Handle -> XML.Parser -> IO XmlTree
getTree' h p = do
	events <- readEventsUntil finished h p 1000
	return $ eventsToTree events
	where
		finished 0 (XML.EndElement _) = True
		finished _ _ = False

putTree :: Stream -> XmlTree -> IO ()
putTree s t = do
	let root = XN.mkRoot [] [t]
	let h = streamHandle s
	[text] <- A.runX (A.constA root >>> A.writeDocumentToString [
		(A.a_no_xml_pi, "1")
		])
	IO.hPutStr h text
	IO.hFlush h

-------------------------------------------------------------------------------

readEventsUntil :: (Int -> XML.Event -> Bool) -> IO.Handle -> XML.Parser -> Int -> IO [XML.Event]
readEventsUntil done h parser timeout = readEventsUntil' done 0 [] $ do
	char <- IO.hGetChar h
	XML.incrementalParse parser [char]

readEventsUntil' done depth accum getEvents = do
	events <- getEvents
	let (done', depth', accum') = readEventsStep done events depth accum
	if done'
		then return accum'
		else readEventsUntil' done depth' accum' getEvents

readEventsStep _ [] depth accum = (False, depth, accum)
readEventsStep done (e:es) depth accum = let
	depth' = depth + case e of
		(XML.BeginElement _ _) -> 1
		(XML.EndElement _) -> (- 1)
		otherwise -> 0
	accum' = accum ++ [e]
	in if done depth' e then (True, depth', accum')
	else readEventsStep done es depth' accum'
