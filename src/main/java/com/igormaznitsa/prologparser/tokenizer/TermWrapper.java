package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;

final class TermWrapper extends PrologTerm {

  private static final long serialVersionUID = 9006607815982718325L;

  private PrologTerm term;

  TermWrapper(final PrologTerm term) {
    super("termWrapper");
    this.term = term;
  }

  PrologTerm getTerm() {
    return this.term;
  }

  void setTerm(final PrologTerm term) {
    this.term = term;
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
