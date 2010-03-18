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
		,streamID
	 	,streamFeatures
	 	)
	,XMPPStreamID(XMPPStreamID)
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
import Data.Char (toUpper)
import Control.Applicative

-- XML Parsing
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.LibXML.SAX as SAX

-- TLS support
import qualified Network.GnuTLS as GnuTLS
import Foreign (allocaBytes)
import Foreign.C (peekCAStringLen)

import Network.Protocol.XMPP.JID (JID, jidFormat)
import qualified Network.Protocol.XMPP.Util as Util

maxXMPPVersion :: XMPPVersion
maxXMPPVersion = XMPPVersion 1 0

data Stream = Stream
	{
		 streamHandle   :: Handle
		,streamJID      :: JID
		,streamNS       :: String
		,streamParser   :: SAX.Parser
		,streamLanguage :: XMLLanguage
		,streamVersion  :: XMPPVersion
		,streamID       :: XMPPStreamID
		,streamFeatures :: [StreamFeature]
	}

data StreamFeature =
	  FeatureStartTLS Bool
	| FeatureSASL [String]
	| FeatureRegister
	| FeatureBind
	| FeatureSession
	| FeatureUnknown DOM.XmlTree
	deriving (Show, Eq)

newtype XMLLanguage = XMLLanguage String
	deriving (Show, Eq)

data XMPPVersion = XMPPVersion Int Int
	deriving (Show, Eq)

newtype XMPPStreamID = XMPPStreamID String

data Handle =
	  PlainHandle IO.Handle
	| SecureHandle IO.Handle (GnuTLS.Session GnuTLS.Client)

------------------------------------------------------------------------------

restartStream :: Stream -> IO Stream
restartStream s = beginStream' (streamJID s) (streamNS s) (streamHandle s)

beginStream :: JID -> String -> IO.Handle -> IO Stream
beginStream jid ns rawHandle = do
	IO.hSetBuffering rawHandle IO.NoBuffering
	
	plainStream <- beginStream' jid ns (PlainHandle rawHandle)

	let startTLS = do
	      putTree plainStream $ Util.mkElement ("", "starttls")
				    [("", "xmlns", "urn:ietf:params:xml:ns:xmpp-tls")]
				    []
	      getTree plainStream
	
	      session <- GnuTLS.tlsClient [
				GnuTLS.handle GnuTLS.:= rawHandle
			       ,GnuTLS.priorities GnuTLS.:= [GnuTLS.CrtX509]
			       ,GnuTLS.credentials GnuTLS.:= GnuTLS.certificateCredentials
			       ]
	      GnuTLS.handshake session
	      beginStream' jid ns (SecureHandle rawHandle session)

	case streamCanTLS plainStream of
	  True -> startTLS
	  False -> return plainStream

beginStream' :: JID -> String -> Handle -> IO Stream
beginStream' jid ns h = do
	-- Since only the opening tag should be written, normal XML
	-- serialization cannot be used. Be careful to escape any embedded
	-- attributes.
	let xmlHeader =
		"<?xml version='1.0'?>\n" ++
		"<stream:stream xmlns='" ++ DOM.attrEscapeXml ns ++ "'" ++
		" to='" ++ (DOM.attrEscapeXml . jidFormat) jid ++ "'" ++
		" version='1.0'" ++
		" xmlns:stream='http://etherx.jabber.org/streams'>"
	
	parser <- SAX.mkParser
	hPutStr h xmlHeader
	initialEvents <- readEventsUntil startOfStream h parser
	
	let startStreamEvent = last initialEvents
	let (language, version, streamID) = parseStartStream startStreamEvent
	features <- (case ns of
		       "jabber:client" ->
			   parseFeatures <$> getTree' h parser
		       _ ->
			   return []
		    )
	
	return $ Stream h jid ns parser language version streamID features
	
	where
		streamName = Util.mkQName "http://etherx.jabber.org/streams" "stream"
		
		startOfStream depth event = case (depth, event) of
			(1, (SAX.BeginElement elemName _)) ->
				streamName == Util.convertQName elemName
			_ -> False

parseStartStream :: SAX.Event -> (XMLLanguage, XMPPVersion, XMPPStreamID)
parseStartStream e = (XMLLanguage lang, XMPPVersion 1 0, XMPPStreamID id)
    where SAX.BeginElement _ attrs = e
	  attr name = maybe "" SAX.attributeValue $
		      m1 $ filter ((name ==) . SAX.qnameLocalName . SAX.attributeName) attrs
	      where m1 (x:_) = Just x
		    m1 _ = Nothing
	  lang = attr "lang"
	  id = attr "id"

parseFeatures :: DOM.XmlTree -> [StreamFeature]
parseFeatures t =
	A.runLA (A.getChildren
		>>> A.hasQName featuresName
		>>> A.getChildren
		>>> A.arrL (\t' -> [parseFeature t'])) t
	where
		featuresName = Util.mkQName "http://etherx.jabber.org/streams" "features"

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
	mechName = Util.mkQName "urn:ietf:params:xml:ns:xmpp-sasl" "mechanism"
	mechanisms = A.runLA (
		A.getChildren
		>>> A.hasQName mechName
		>>> A.getChildren
		>>> A.getText) t
	
	in FeatureSASL $ map (map toUpper) mechanisms

streamCanTLS :: Stream -> Bool
streamCanTLS = (> 0) . length .
	       filter (\feature ->
			   case feature of
			     FeatureStartTLS _ -> True
			     _ -> False
		      ) . streamFeatures

-------------------------------------------------------------------------------

getTree :: Stream -> IO DOM.XmlTree
getTree s = getTree' (streamHandle s) (streamParser s)

getTree' :: Handle -> SAX.Parser -> IO DOM.XmlTree
getTree' h p = do
	events <- readEventsUntil finished h p
	return $ Util.eventsToTree events
	where
		finished 0 (SAX.EndElement _) = True
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

readEventsUntil :: (Int -> SAX.Event -> Bool) -> Handle -> SAX.Parser -> IO [SAX.Event]
readEventsUntil done h parser = readEventsUntil' done 0 [] $ do
	char <- hGetChar h
	SAX.parse parser [char] False

readEventsUntil' :: (Int -> SAX.Event -> Bool) -> Int -> [SAX.Event] -> IO [SAX.Event] -> IO [SAX.Event]
readEventsUntil' done depth accum getEvents = do
	events <- getEvents
	let (done', depth', accum') = readEventsStep done events depth accum
	if done'
		then return accum'
		else readEventsUntil' done depth' accum' getEvents

readEventsStep :: (Int -> SAX.Event -> Bool) -> [SAX.Event] -> Int -> [SAX.Event] -> (Bool, Int, [SAX.Event])
readEventsStep _ [] depth accum = (False, depth, accum)
readEventsStep done (e:es) depth accum = let
	depth' = depth + case e of
		(SAX.BeginElement _ _) -> 1
		(SAX.EndElement _) -> (- 1)
		_ -> 0
	accum' = accum ++ [e]
	in if done depth' e then (True, depth', accum')
	else readEventsStep done es depth' accum'

-------------------------------------------------------------------------------

hPutStr :: Handle -> String -> IO ()
hPutStr (PlainHandle h) = IO.hPutStr h
hPutStr (SecureHandle _ session) = GnuTLS.tlsSendString session

hGetChar :: Handle -> IO Char
hGetChar (PlainHandle h) = IO.hGetChar h
hGetChar (SecureHandle h session) = allocaBytes 1 $ \ptr -> do
	pending <- GnuTLS.tlsCheckPending session
	if pending == 0
		then do
			IO.hWaitForInput h (-1)
			return ()
		else return ()
	
	len <- GnuTLS.tlsRecv session ptr 1
	[char] <- peekCAStringLen (ptr, len)
	return char
