package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;

final class TermWrapper extends AbstractPrologTerm {

  private static final long serialVersionUID = 9006607815982718325L;

  private volatile AbstractPrologTerm term;

  TermWrapper(final AbstractPrologTerm term) {
    super("termWrapper");
    this.term = term;
  }

  AbstractPrologTerm getTerm() {
    return this.term;
  }

  void setTerm(final AbstractPrologTerm term) {
    this.term = term;
  }

  @Override
  public PrologTermType getType() {
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
