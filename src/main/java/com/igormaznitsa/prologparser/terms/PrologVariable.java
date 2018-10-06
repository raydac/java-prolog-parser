package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.AssertUtils;

public final class PrologVariable extends PrologTerm {
  private static final long serialVersionUID = 1158349084517573220L;
  private final boolean anonymous;

  public PrologVariable() {
    this("_");
  }

  public PrologVariable(final int line, final int pos) {
    this();
    setPos(pos);
    setLine(line);
  }

  public PrologVariable(final String text) {
    super(text);

    final char startWith = AssertUtils.assertStringNotNullAndNotEmpty(text).charAt(0);

    if (!Character.isUpperCase(startWith) && startWith != '_') {
      throw new IllegalArgumentException("Var must start with upper case char or '_' [" + text + ']');
    }

    this.anonymous = text.length() == 1 && startWith == '_';
  }

  public PrologVariable(final String text, final int line, final int pos) {
    this(text);
    setPos(pos);
    setLine(line);
  }

  public boolean isAnonymous() {
    return anonymous;
  }

  @Override
  public PrologTermType getType() {
    return PrologTermType.VAR;
  }
}
