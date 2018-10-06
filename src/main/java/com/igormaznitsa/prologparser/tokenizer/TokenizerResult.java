package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.AssertUtils;

final class TokenizerResult {
  private final TokenizerState parserState;
  private final PrologTerm resultTerm;
  private final int pos;
  private final int line;

  TokenizerResult(final PrologTerm term, final TokenizerState parserState, final int line, final int pos) {
    this.resultTerm = AssertUtils.assertNotNull(term);
    this.parserState = AssertUtils.assertNotNull(parserState);
    this.pos = pos;
    this.line = line;
  }

  TokenizerState getTokenizerState() {
    return parserState;
  }

  PrologTerm getResult() {
    return resultTerm;
  }

  PrologTermType getTermType() {
    return resultTerm.getType();
  }

  int getStringPosition() {
    return pos;
  }

  int getLine() {
    return line;
  }
}
