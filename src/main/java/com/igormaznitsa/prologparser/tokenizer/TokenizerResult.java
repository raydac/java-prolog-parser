package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;

final class TokenizerResult {
  private final TokenizerState parserState;
  private final AbstractPrologTerm resultTerm;
  private final int stringPosition;
  private final int lineNumber;

  TokenizerResult(final AbstractPrologTerm term,
                  final TokenizerState parserState,
                  final int stringPosition,
                  final int lineNumber) {
    if (term == null) {
      throw new NullPointerException("Term is null");
    }
    if (parserState == null) {
      throw new NullPointerException("Parser state is null");
    }

    this.stringPosition = stringPosition;
    this.lineNumber = lineNumber;
    this.resultTerm = term;
    this.parserState = parserState;
  }

  TokenizerState getTokenizerState() {
    return parserState;
  }

  AbstractPrologTerm getResult() {
    return resultTerm;
  }

  PrologTermType getTermType() {
    return resultTerm.getType();
  }

  int getStringPosition() {
    return stringPosition;
  }

  int getLineNumber() {
    return lineNumber;
  }
}
