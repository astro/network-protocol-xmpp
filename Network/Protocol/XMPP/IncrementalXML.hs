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

{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Protocol.XMPP.IncrementalXML (
	 Parser
	,Event(..)
	,Attribute(..)
	,newParser
	,incrementalParse
	) where

import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Foreign.C (CInt, CString, withCStringLen, peekCString, peekCStringLen)
import qualified Foreign as F
import Control.Exception (bracket)
import Text.XML.HXT.DOM.QualifiedName (mkQName, QName)

data ParserStruct = ParserStruct
data Parser = Parser !(F.ForeignPtr ParserStruct)

data Event =
	  BeginElement QName [Attribute]
	| EndElement QName
	| Characters String
	| ParseError String
	deriving (Show, Eq)

data Attribute = Attribute QName String
	deriving (Show, Eq)

newParser :: IO Parser
newParser = do
	ptr <- c_incremental_parser_new
	autoptr <- F.newForeignPtr c_incremental_parser_free ptr
	return $ Parser autoptr

incrementalParse :: Parser -> String -> IO [Event]
incrementalParse (Parser autoptr) s = do
	events <- newIORef []
	
	withCStringLen s $ \(cs, cs_len) -> do
	F.withForeignPtr autoptr $ \ptr -> do
	withFunPtr (onBeginElement events) wrappedBegin $ \b -> do
	withFunPtr (onEndElement events) wrappedEnd $ \e -> do
	withFunPtr (onCharacters events) wrappedText $ \t -> do
		retval <- (c_incremental_parse ptr cs (fromIntegral cs_len) b e t)
		(readIORef events) >>= (return . checkReturn retval)

checkReturn :: CInt -> [Event] -> [Event]
checkReturn r es = es ++ case r of
	0 -> []
	_ -> [ParseError (show r)]

withFunPtr :: a -> (a -> IO (F.FunPtr a)) -> (F.FunPtr a -> IO b) -> IO b
withFunPtr f mkPtr block = bracket (mkPtr f) F.freeHaskellFunPtr block

-- localname, prefix, namespace, value_begin, value_end
data CAttribute = CAttribute CString CString CString CString CString

splitCAttributes :: CInt -> F.Ptr CString -> IO [CAttribute]
splitCAttributes = splitCAttributes' 0

splitCAttributes' _      0 _     = return []
splitCAttributes' offset n attrs = do
	c_ln <- F.peekElemOff attrs (offset + 0)
	c_prefix <- F.peekElemOff attrs (offset + 1)
	c_ns <- F.peekElemOff attrs (offset + 2)
	c_vbegin <- F.peekElemOff attrs (offset + 3)
	c_vend <- F.peekElemOff attrs (offset + 4)
	as <- splitCAttributes' (offset + 5) (n - 1) attrs
	return (CAttribute c_ln c_prefix c_ns c_vbegin c_vend : as)

convertCAttribute :: CAttribute -> IO Attribute
convertCAttribute (CAttribute c_ln c_pfx c_ns c_vbegin c_vend) = do
	ln <- peekCString c_ln
	pfx <- peekNullable c_pfx
	ns <- peekNullable c_ns
	val <- peekCStringLen (c_vbegin, F.minusPtr c_vend c_vbegin)
	return (Attribute (mkQName pfx ln ns) val)

peekNullable :: CString -> IO String
peekNullable ptr
	| ptr == F.nullPtr = return ""
	| otherwise        = peekCString ptr

onBeginElement :: IORef [Event] -> F.Ptr () -> CString -> CString -> CString -> CInt -> F.Ptr () -> CInt -> CInt -> F.Ptr CString -> IO ()
onBeginElement eventref _ cln cpfx cns _ _ n_attrs _ raw_attrs = do
	ns <- peekCString cns
	pfx <- peekNullable cpfx
	ln <- peekCString cln
	es <- readIORef eventref
	c_attrs <- splitCAttributes n_attrs raw_attrs
	attrs <- mapM convertCAttribute c_attrs
	writeIORef eventref (es ++ [BeginElement (mkQName pfx ln ns) attrs])

onEndElement :: IORef [Event] -> F.Ptr () -> CString -> CString -> CString -> IO ()
onEndElement eventref _ cln cpfx cns = do
	ns <- peekCString cns
	pfx <- peekNullable cpfx
	ln <- peekCString cln
	es <- readIORef eventref
	writeIORef eventref (es ++ [EndElement (mkQName pfx ln ns)])

onCharacters :: IORef [Event] -> F.Ptr () -> CString -> CInt -> IO ()
onCharacters eventref _ ctext ctextlen = do
	text <- (peekCStringLen (ctext, fromIntegral ctextlen))
	es <- readIORef eventref
	writeIORef eventref (es ++ [Characters text])

type StartElementNsSAX2Func = (F.Ptr () -> CString -> CString -> CString -> CInt -> F.Ptr () -> CInt -> CInt -> F.Ptr CString -> IO ())
type EndElementNsSAX2Func = (F.Ptr () -> CString -> CString -> CString -> IO ())
type CharactersSAXFunc = (F.Ptr () -> CString -> CInt -> IO ())

foreign import ccall "wrapper"
	wrappedBegin :: StartElementNsSAX2Func -> IO (F.FunPtr StartElementNsSAX2Func)

foreign import ccall "wrapper"
	wrappedEnd :: EndElementNsSAX2Func -> IO (F.FunPtr EndElementNsSAX2Func)

foreign import ccall "wrapper"
	wrappedText :: CharactersSAXFunc -> IO (F.FunPtr CharactersSAXFunc)

foreign import ccall "incremental-xml.h incremental_parser_new"
	c_incremental_parser_new :: IO (F.Ptr ParserStruct)

foreign import ccall "incremental-xml.h incremental_parse"
	c_incremental_parse :: F.Ptr ParserStruct -> CString -> CInt
	                    -> F.FunPtr StartElementNsSAX2Func
	                    -> F.FunPtr EndElementNsSAX2Func
	                    -> F.FunPtr CharactersSAXFunc
	                    -> IO CInt

foreign import ccall "incremental-xml.h &incremental_parser_free"
	c_incremental_parser_free :: F.FunPtr (F.Ptr ParserStruct -> IO ())

