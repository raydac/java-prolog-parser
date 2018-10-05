package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.tokenizer.PrologParser;

import java.io.Reader;

@SuppressWarnings("serial")
public class GenericPrologParser extends PrologParser {
  public GenericPrologParser(Reader reader, ParserContext context) {
    super(reader, context);
  }
}
