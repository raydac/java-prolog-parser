package com.igormaznitsa.prologparser.terms;

public final class PrologVariable extends AbstractPrologTerm {
  private static final long serialVersionUID = 1058349084517573220L;
  private final boolean is_anonymous;

  public PrologVariable() {
    this("_");
  }

  public PrologVariable(final int strPosition, final int lineNumber) {
    this();
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  public PrologVariable(final String text) {
    super(text);

    if (text.isEmpty()) {
      throw new IllegalArgumentException("Variable name is empty");
    }

    final char firstLetter = text.charAt(0);

    if (!Character.isUpperCase(firstLetter) && firstLetter != '_') {
      throw new IllegalArgumentException(
          "The variable name must be started from an upper case letter or '_' ["
              + text + ']');
    }

    is_anonymous = text.length() == 1 && firstLetter == '_';
  }

  public PrologVariable(final String text, final int strPosition, final int lineNumber) {
    this(text);
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  public boolean isAnonymous() {
    return is_anonymous;
  }

  @Override
  public PrologTermType getType() {
    return PrologTermType.VAR;
  }
}
