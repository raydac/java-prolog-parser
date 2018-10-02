package com.igormaznitsa.prologparser.terms;

public abstract class AbstractPrologNumericTerm extends AbstractPrologTerm {

  private static final long serialVersionUID = -1865562758090770438L;

  public AbstractPrologNumericTerm() {
    super("");
  }

  public AbstractPrologNumericTerm(final int strPosition, final int lineNumber) {
    super("", strPosition, lineNumber);
  }

  @Override
  public final PrologTermType getType() {
    return PrologTermType.ATOM;
  }

  @Override
  public String getText() {
    return toString();
  }

  public abstract AbstractPrologNumericTerm neg();
}
