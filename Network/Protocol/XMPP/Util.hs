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
	,mkElement
	,mkAttr
	,mkQName
	) where

import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.QualifiedName as QN
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import qualified Network.Protocol.XMPP.IncrementalXML as XML

-------------------------------------------------------------------------------
-- For converting incremental XML event lists to HXT trees
-------------------------------------------------------------------------------

-- This function assumes the input list is valid. No validation is performed.
eventsToTree :: [XML.Event] -> XmlTree
eventsToTree es = XN.mkRoot [] (eventsToTrees es)

eventsToTrees :: [XML.Event] -> [XmlTree]
eventsToTrees es = map blockToTree (splitBlocks es)

-- Split event list into a sequence of "blocks", which are the events including
-- and between a pair of tags. <start><start2/></start> and <start/> are both
-- single blocks.
splitBlocks :: [XML.Event] -> [[XML.Event]]
splitBlocks es = ret where (_, _, ret) = foldl splitBlocks' (0, [], []) es

splitBlocks' (depth, accum, allAccum) e =
	if depth' == 0 then
		(depth', [], allAccum ++ [accum'])
	else
		(depth', accum', allAccum)
	where
		accum' = accum ++ [e]
		depth' = depth + case e of
			(XML.BeginElement _ _) -> 1
			(XML.EndElement _) -> (- 1)
			otherwise -> 0

blockToTree :: [XML.Event] -> XmlTree
blockToTree (begin:rest) = let end = (last rest) in case (begin, end) of
	(XML.BeginElement qname attrs, XML.EndElement _) ->
		XN.mkElement qname (map convertAttr attrs) (eventsToTrees (init rest))
	(XML.Characters s, _) -> XN.mkText s
	(_, XML.ParseError _) -> undefined
	fff -> error ("Got unexpected: " ++ (show fff))

convertAttr :: XML.Attribute -> XmlTree
convertAttr (XML.Attribute qname value) = XN.NTree (XN.mkAttrNode qname) [XN.mkText value]

-------------------------------------------------------------------------------
-- Utility function for building XML trees
-------------------------------------------------------------------------------

mkElement :: (String, String) -> [(String, String, String)] -> [XmlTree] -> XmlTree
mkElement (ns, localpart) attrs children = let
	qname = mkQName ns localpart
	attrs' = [mkAttr ans alp text | (ans, alp, text) <- attrs]
	in XN.mkElement qname attrs' children

mkAttr ns localpart text = XN.mkAttr (mkQName ns localpart) [XN.mkText text]

mkQName ns localpart = case ns of
	"" -> QN.mkName localpart
	otherwise -> QN.mkNsName ns localpart
