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
	 Mechanism
	,supportedMechanisms
	,bestMechanism
	,findMechanism
	) where

import Data.List (intersect)
import Data.AssocList (lookupDef)

type Username = String
type Password = String

type Mechanism = String

-- TODO: validation
supportedMechanisms :: [Mechanism]
supportedMechanisms = ["PLAIN"] -- TODO: Digest-MD5

bestMechanism :: [Mechanism] -> Maybe Mechanism
bestMechanism ms = let
	in case intersect supportedMechanisms ms of
		[] -> Nothing
		(m:_) -> Just m

findMechanism :: String -> Mechanism
findMechanism s = s -- TODO: validate
