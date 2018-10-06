package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.tokenizer.PrologParser;

import java.io.Reader;

@SuppressWarnings("serial")
public class GenericPrologParser extends PrologParser {
  public GenericPrologParser(final Reader reader, final ParserContext context) {
    super(reader, context);
  }

  public GenericPrologParser(final Reader reader) {
    this(reader, null);
  }
}
