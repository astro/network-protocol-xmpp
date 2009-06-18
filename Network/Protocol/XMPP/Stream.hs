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
		,FeatureBind
		,FeatureSession
		)
	,beginStream
	,restartStream
	,getTree
	,putTree
	) where

import qualified System.IO as IO
import Data.AssocList (lookupDef)

-- XML Parsing
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Network.Protocol.XMPP.IncrementalXML as XML

-- TLS support
import qualified Network.GnuTLS as GnuTLS
import Foreign (allocaBytes)
import Foreign.C (peekCAStringLen)

import Network.Protocol.XMPP.JID (JID)
import Network.Protocol.XMPP.SASL (Mechanism, findMechanism)
import Network.Protocol.XMPP.Util (eventsToTree, mkQName, mkElement)

maxXMPPVersion :: XMPPVersion
maxXMPPVersion = XMPPVersion 1 0

data Stream = Stream
	{
		 streamHandle   :: Handle
		,streamJID      :: JID
		,streamParser   :: XML.Parser
		,streamLanguage :: XMLLanguage
		,streamVersion  :: XMPPVersion
		,streamFeatures :: [StreamFeature]
	}

data StreamFeature =
	  FeatureStartTLS Bool
	| FeatureSASL [Mechanism]
	| FeatureRegister
	| FeatureBind
	| FeatureSession
	| FeatureUnknown DOM.XmlTree
	deriving (Show, Eq)

newtype XMLLanguage = XMLLanguage String
	deriving (Show, Eq)

data XMPPVersion = XMPPVersion Int Int
	deriving (Show, Eq)

data Handle =
	  PlainHandle IO.Handle
	| SecureHandle (GnuTLS.Session GnuTLS.Client)

------------------------------------------------------------------------------

restartStream :: Stream -> IO Stream
restartStream s = beginStream' (streamJID s) (streamHandle s)

beginStream :: JID -> IO.Handle -> IO Stream
beginStream jid rawHandle = do
	IO.hSetBuffering rawHandle IO.NoBuffering
	
	plainStream <- beginStream' jid (PlainHandle rawHandle)
	
	putTree plainStream $ mkElement ("", "starttls")
		[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-tls")]
		[]
	getTree plainStream
	
	session <- GnuTLS.tlsClient [
		 GnuTLS.handle GnuTLS.:= rawHandle
		,GnuTLS.priorities GnuTLS.:= [GnuTLS.CrtX509]
		,GnuTLS.credentials GnuTLS.:= GnuTLS.certificateCredentials
		]
	GnuTLS.handshake session
	beginStream' jid (SecureHandle session)

beginStream' :: JID -> Handle -> IO Stream
beginStream' jid h = do
	-- Since only the opening tag should be written, normal XML
	-- serialization cannot be used. Be careful to escape any embedded
	-- attributes.
	let xmlHeader =
		"<?xml version='1.0'?>\n" ++
		"<stream:stream xmlns='jabber:client'" ++
		" to='" ++ (DOM.attrEscapeXml . show) jid ++ "'" ++
		" version='1.0'" ++
		" xmlns:stream='http://etherx.jabber.org/streams'>"
	
	parser <- XML.newParser
	hPutStr h xmlHeader
	[startStreamEvent] <- readEventsUntil startOfStream h parser
	featureTree <- getTree' h parser
	
	let (language, version) = parseStartStream startStreamEvent
	let features = parseFeatures featureTree
	
	return $ Stream h jid parser language version features
	
	where
		streamName = mkQName "http://etherx.jabber.org/streams" "stream"
		
		startOfStream depth event = case (depth, event) of
			(1, (XML.BeginElement elemName _)) -> streamName == elemName
			_ -> False

parseStartStream :: XML.Event -> (XMLLanguage, XMPPVersion)
parseStartStream e = (XMLLanguage "en", XMPPVersion 1 0) -- TODO

parseFeatures :: DOM.XmlTree -> [StreamFeature]
parseFeatures t =
	A.runLA (A.getChildren
		>>> A.hasQName featuresName
		>>> A.getChildren
		>>> A.arrL (\t' -> [parseFeature t'])) t
	where
		featuresName = mkQName "http://etherx.jabber.org/streams" "features"

parseFeature :: DOM.XmlTree -> StreamFeature
parseFeature t = lookupDef FeatureUnknown qname [
	 (("urn:ietf:params:xml:ns:xmpp-tls", "starttls"), parseFeatureTLS)
	,(("urn:ietf:params:xml:ns:xmpp-sasl", "mechanisms"), parseFeatureSASL)
	,(("http://jabber.org/features/iq-register", "register"), (\_ -> FeatureRegister))
	,(("urn:ietf:params:xml:ns:xmpp-bind", "bind"), (\_ -> FeatureBind))
	,(("urn:ietf:params:xml:ns:xmpp-session", "session"), (\_ -> FeatureSession))
	] t
	where
		qname = maybe ("", "") (\n -> (DOM.namespaceUri n, DOM.localPart n)) (XN.getName t)

parseFeatureTLS :: DOM.XmlTree -> StreamFeature
parseFeatureTLS t = FeatureStartTLS True -- TODO: detect whether or not required

parseFeatureSASL :: DOM.XmlTree -> StreamFeature
parseFeatureSASL t = let
	mechName = mkQName "urn:ietf:params:xml:ns:xmpp-sasl" "mechanism"
	rawMechanisms = A.runLA (
		A.getChildren
		>>> A.hasQName mechName
		>>> A.getChildren
		>>> A.getText) t
	
	-- TODO: validate mechanism names according to SASL rules
	-- <20 chars, uppercase, alphanum, etc
	in FeatureSASL (map findMechanism rawMechanisms)

-------------------------------------------------------------------------------

getTree :: Stream -> IO DOM.XmlTree
getTree s = getTree' (streamHandle s) (streamParser s)

getTree' :: Handle -> XML.Parser -> IO DOM.XmlTree
getTree' h p = do
	events <- readEventsUntil finished h p
	return $ eventsToTree events
	where
		finished 0 (XML.EndElement _) = True
		finished _ _ = False

putTree :: Stream -> DOM.XmlTree -> IO ()
putTree s t = do
	let root = XN.mkRoot [] [t]
	let h = streamHandle s
	[text] <- A.runX (A.constA root >>> A.writeDocumentToString [
		(A.a_no_xml_pi, "1")
		])
	hPutStr h text

-------------------------------------------------------------------------------

readEventsUntil :: (Int -> XML.Event -> Bool) -> Handle -> XML.Parser -> IO [XML.Event]
readEventsUntil done h parser = readEventsUntil' done 0 [] $ do
	char <- hGetChar h
	XML.incrementalParse parser [char]

readEventsUntil' :: (Int -> XML.Event -> Bool) -> Int -> [XML.Event] -> IO [XML.Event] -> IO [XML.Event]
readEventsUntil' done depth accum getEvents = do
	events <- getEvents
	let (done', depth', accum') = readEventsStep done events depth accum
	if done'
		then return accum'
		else readEventsUntil' done depth' accum' getEvents

readEventsStep :: (Int -> XML.Event -> Bool) -> [XML.Event] -> Int -> [XML.Event] -> (Bool, Int, [XML.Event])
readEventsStep _ [] depth accum = (False, depth, accum)
readEventsStep done (e:es) depth accum = let
	depth' = depth + case e of
		(XML.BeginElement _ _) -> 1
		(XML.EndElement _) -> (- 1)
		_ -> 0
	accum' = accum ++ [e]
	in if done depth' e then (True, depth', accum')
	else readEventsStep done es depth' accum'

-------------------------------------------------------------------------------

hPutStr :: Handle -> String -> IO ()
hPutStr (PlainHandle h) = IO.hPutStr h
hPutStr (SecureHandle h) = GnuTLS.tlsSendString h

hGetChar :: Handle -> IO Char
hGetChar (PlainHandle h) = IO.hGetChar h
hGetChar (SecureHandle h) = allocaBytes 1 $ \ptr -> do
	len <- GnuTLS.tlsRecv h ptr 1
	[char] <- peekCAStringLen (ptr, len)
	return char
