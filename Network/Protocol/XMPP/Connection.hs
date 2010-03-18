{- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
                      Stephan Maka <stephan@spaceboyz.net>
   
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

module Network.Protocol.XMPP.Connection
    (Connection
    ,getTree
    ,putTree
    ,putStanza
    ) where

import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Network.Protocol.XMPP.Stanzas (Stanza, stanzaToTree)


-- |Provides the basic operations for XMPP connections.
class Connection c where
    -- |Receive XML
    getTree :: c -> IO XmlTree
    -- |Send XML
    putTree :: c -> XmlTree -> IO ()

    -- |Send a stanza, uses putTree by default
    putStanza :: c -> Stanza -> IO ()
    putStanza c = putTree c . stanzaToTree
