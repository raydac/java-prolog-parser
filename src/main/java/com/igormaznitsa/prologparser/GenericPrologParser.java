package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.tokenizer.AbstractPrologParser;

@SuppressWarnings("serial")
public class GenericPrologParser extends AbstractPrologParser {
  public GenericPrologParser(CharSource source, ParserContext context) {
    super(source, context);
  }
}
