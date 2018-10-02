package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCache;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCacheItem;

final class PrologTermWrapper extends AbstractPrologTerm implements SoftCacheItem {

  private static final long serialVersionUID = 9006607815982718325L;

  private volatile AbstractPrologTerm wrappedTerm;

  private transient volatile SoftCache<PrologTermWrapper> ringBuffer;

  PrologTermWrapper() {
    super("termWrapper");
  }

  public AbstractPrologTerm getWrappedTerm() {
    return this.wrappedTerm;
  }

  public void setWrappedTerm(final AbstractPrologTerm term) {
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
    this.ringBuffer = (SoftCache<PrologTermWrapper>) ringBuffer;
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
