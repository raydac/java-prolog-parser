package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCache;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCacheItem;

public final class TokenizerResult implements SoftCacheItem {

  private SoftCache<? extends SoftCacheItem> ringBuffer;

  private TokenizerState parserState;
  private AbstractPrologTerm resultTerm;
  private int stringPosition;
  private int lineNumber;


  TokenizerResult() {
  }

  TokenizerResult(final AbstractPrologTerm term,
                  final TokenizerState parserState,
                  final int stringPosition,
                  final int lineNumber) {
    setData(term, parserState, stringPosition, lineNumber);
  }

  @Override
  public void setCache(final SoftCache<? extends SoftCacheItem> owner) {
    this.ringBuffer = owner;
  }

  @Override
  public void release() {
    if (ringBuffer != null) {
      ringBuffer.tryPush(this);
    }
  }

  @Override
  public void reset() {
    this.resultTerm = null;
    this.parserState = null;
    this.lineNumber = -1;
    this.stringPosition = -1;
  }

  void setData(final AbstractPrologTerm term,
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

  public TokenizerState getTokenizerState() {
    return parserState;
  }

  public AbstractPrologTerm getResult() {
    return resultTerm;
  }

  public PrologTermType getTermType() {
    return resultTerm.getType();
  }

  public int getStringPosition() {
    return stringPosition;
  }

  public int getLineNumber() {
    return lineNumber;
  }
}
