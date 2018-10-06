package com.igormaznitsa.prologparser.terms;

public abstract class PrologNumeric extends PrologTerm {

  private static final long serialVersionUID = -1815562758090770438L;

  public PrologNumeric() {
    super("");
  }

  public PrologNumeric(final int line, final int pos) {
    super("", line, pos);
  }

  @Override
  public final TermType getType() {
    return TermType.ATOM;
  }

  @Override
  public String getText() {
    return toString();
  }

  public abstract PrologNumeric neg();
}
