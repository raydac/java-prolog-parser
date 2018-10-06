package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.utils.AssertUtils;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;

final class TokenizerResult {
  private final SoftObjectPool<TokenizerResult> pool;
  private TokenizerState parserState;
  private PrologTerm resultTerm;
  private int pos;
  private int line;

  TokenizerResult(final SoftObjectPool<TokenizerResult> pool) {
    this.pool = pool;
  }

  TokenizerResult setData(final PrologTerm term, final TokenizerState parserState, final int line, final int pos) {
    this.resultTerm = AssertUtils.assertNotNull(term);
    this.parserState = AssertUtils.assertNotNull(parserState);
    this.pos = pos;
    this.line = line;
    return this;
  }

  void release() {
    this.parserState = null;
    this.resultTerm = null;
    this.pos = -1;
    this.line = -1;
    this.pool.push(this);
  }

  TokenizerState getTokenizerState() {
    return this.parserState;
  }

  PrologTerm getResult() {
    return this.resultTerm;
  }

  int getPos() {
    return this.pos;
  }

  int getLine() {
    return this.line;
  }
}
