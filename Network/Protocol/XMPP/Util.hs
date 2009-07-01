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

module Network.Protocol.XMPP.Util (
	 eventsToTree
	,convertAttr
	,convertQName
	,mkElement
	,mkAttr
	,mkQName
	) where

import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.LibXML.SAX as SAX

-------------------------------------------------------------------------------
-- For converting incremental XML event lists to HXT trees
-------------------------------------------------------------------------------

-- This function assumes the input list is valid. No validation is performed.
eventsToTree :: [SAX.Event] -> DOM.XmlTree
eventsToTree es = XN.mkRoot [] (eventsToTrees es)

eventsToTrees :: [SAX.Event] -> [DOM.XmlTree]
eventsToTrees es = concatMap blockToTrees (splitBlocks es)

-- Split event list into a sequence of "blocks", which are the events including
-- and between a pair of tags. <start><start2/></start> and <start/> are both
-- single blocks.
splitBlocks :: [SAX.Event] -> [[SAX.Event]]
splitBlocks es = ret where (_, _, ret) = foldl splitBlocks' (0, [], []) es

splitBlocks' :: (Int, [SAX.Event], [[SAX.Event]])
                -> SAX.Event
                -> (Int, [SAX.Event], [[SAX.Event]])
splitBlocks' (depth, accum, allAccum) e =
	if depth' == 0 then
		(depth', [], allAccum ++ [accum'])
	else
		(depth', accum', allAccum)
	where
		accum' = accum ++ [e]
		depth' = depth + case e of
			(SAX.BeginElement _ _) -> 1
			(SAX.EndElement _) -> (- 1)
			_ -> 0

blockToTrees :: [SAX.Event] -> [DOM.XmlTree]
blockToTrees [] = []
blockToTrees (begin:rest) = let end = (last rest) in case (begin, end) of
	(SAX.BeginElement qname attrs, SAX.EndElement _) ->
		[XN.mkElement (convertQName qname)
			(map convertAttr attrs)
			(eventsToTrees (init rest))]
	(SAX.Characters s, _) -> [XN.mkText s]
	(_, SAX.ParseError text) -> error text
	_ -> []

convertAttr :: SAX.Attribute -> DOM.XmlTree
convertAttr (SAX.Attribute qname value) = XN.NTree
	(XN.mkAttrNode (convertQName qname))
	[XN.mkText value]

convertQName :: SAX.QName -> DOM.QName
convertQName (SAX.QName ns _ local) = mkQName ns local

-------------------------------------------------------------------------------
-- Utility function for building XML trees
-------------------------------------------------------------------------------

mkElement :: (String, String) -> [(String, String, String)] -> [DOM.XmlTree] -> DOM.XmlTree
mkElement (ns, localpart) attrs children = let
	qname = mkQName ns localpart
	attrs' = [mkAttr ans alp text | (ans, alp, text) <- attrs]
	in XN.mkElement qname attrs' children

mkAttr :: String -> String -> String -> DOM.XmlTree
mkAttr ns localpart text = XN.mkAttr (mkQName ns localpart) [XN.mkText text]

mkQName :: String -> String -> DOM.QName
mkQName ns localpart = case ns of
	"" -> DOM.mkName localpart
	_ -> DOM.mkNsName localpart ns
