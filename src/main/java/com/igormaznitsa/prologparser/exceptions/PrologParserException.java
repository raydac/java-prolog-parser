package com.igormaznitsa.prologparser.exceptions;

public class PrologParserException extends Exception {
  private static final long serialVersionUID = -4454323844625857706L;

  protected final int lineNumber;
  protected final int stringPosition;

  public PrologParserException(final String text, final int lineNumber,
                               final int stringPos) {
    super(text);
    this.lineNumber = lineNumber;
    this.stringPosition = stringPos;
  }

  public int getLineNumber() {
    return lineNumber;
  }

  public int getStringPosition() {
    return stringPosition;
  }

  public boolean containsRightPositionData() {
    return lineNumber > 0 && stringPosition > 0;
  }

  @Override
  public String toString() {
    return String.format("%s[%d:%d]", this.getMessage(), this.lineNumber, this.stringPosition);
  }
}
