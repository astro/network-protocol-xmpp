/* Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
   
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
*/

#include <string.h>
#include <assert.h>
#include "incremental-xml.h"

struct _IncrementalParser
{
	xmlSAXHandler handler;
	xmlParserCtxt *context;
};

IncrementalParser *
incremental_parser_new ()
{
	IncrementalParser *parser;
	parser = malloc (sizeof (IncrementalParser));
	assert (parser != NULL);
	
	memset (&(parser->handler), 0, sizeof (parser->handler));
	parser->handler.initialized = XML_SAX2_MAGIC;
	
	parser->context = xmlCreatePushParserCtxt (
		&(parser->handler), parser, NULL, 0, NULL);
	assert (parser->context != NULL);
	
	return parser;
}

void
incremental_parser_free (IncrementalParser *p)
{
	xmlClearParserCtxt (p->context);
	xmlFreeParserCtxt (p->context);
	free (p);
}

int
incremental_parse (
	IncrementalParser *parser,
	const char *text,
	int text_len,
	startElementNsSAX2Func begin,
	endElementNsSAX2Func end,
	charactersSAXFunc text_handler)
{
	xmlParserCtxt *c = parser->context;
	c->sax->startElementNs = begin;
	c->sax->endElementNs = end;
	c->sax->characters = text_handler;
	return xmlParseChunk (c, text, text_len, 0);
}
