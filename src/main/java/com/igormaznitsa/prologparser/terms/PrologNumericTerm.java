package com.igormaznitsa.prologparser.terms;

public abstract class PrologNumericTerm extends PrologTerm {

  private static final long serialVersionUID = -1815562758090770438L;

  public PrologNumericTerm() {
    super("");
  }

  public PrologNumericTerm(final int line, final int pos) {
    super("", line, pos);
  }

  @Override
  public final PrologTermType getType() {
    return PrologTermType.ATOM;
  }

  @Override
  public String getText() {
    return toString();
  }

  public abstract PrologNumericTerm neg();
}
