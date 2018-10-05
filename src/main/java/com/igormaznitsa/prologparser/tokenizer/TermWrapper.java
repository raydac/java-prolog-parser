package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCache;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCacheItem;

final class TermWrapper extends AbstractPrologTerm implements SoftCacheItem {

  private static final long serialVersionUID = 9106607815982718325L;

  private volatile AbstractPrologTerm wrappedTerm;

  private transient volatile SoftCache<TermWrapper> ringBuffer;

  TermWrapper() {
    super("termWrapper");
  }

  AbstractPrologTerm getWrappedTerm() {
    return this.wrappedTerm;
  }

  void setWrappedTerm(final AbstractPrologTerm term) {
    this.wrappedTerm = term;
  }

  @Override
  public PrologTermType getType() {
    return wrappedTerm.getType();
  }

  @Override
  public String getText() {
    return wrappedTerm.getText();
  }

  @Override
  public int getPrecedence() {
    return wrappedTerm.getPrecedence();
  }

  @Override
  public String toString() {
    return wrappedTerm.toString();
  }

  @Override
  @SuppressWarnings("unchecked")
  public void setCache(final SoftCache<? extends SoftCacheItem> ringBuffer) {
    this.ringBuffer = (SoftCache<TermWrapper>) ringBuffer;
  }

  @Override
  public void reset() {
    this.wrappedTerm = null;
  }

  @Override
  public void release() {
    if (this.ringBuffer != null) {
      this.ringBuffer.tryPush(this);
    }
  }
}
