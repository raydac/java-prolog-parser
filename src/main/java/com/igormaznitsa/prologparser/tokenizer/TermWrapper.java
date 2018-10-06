package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;

final class TermWrapper extends PrologTerm {

  private static final long serialVersionUID = 9006607815982718325L;
  private final SoftObjectPool<TermWrapper> pool;
  private PrologTerm term;

  TermWrapper(final SoftObjectPool<TermWrapper> pool) {
    super("termWrapper");
    this.pool = pool;
  }

  void release() {
    this.term = null;
    this.pool.push(this);
  }

  PrologTerm getTerm() {
    return this.term;
  }

  TermWrapper setTerm(final PrologTerm term) {
    this.term = term;
    return this;
  }

  @Override
  public TermType getType() {
    return term.getType();
  }

  @Override
  public String getText() {
    return term.getText();
  }

  @Override
  public int getPrecedence() {
    return term.getPrecedence();
  }

  @Override
  public String toString() {
    return term.toString();
  }

}
