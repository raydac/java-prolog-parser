package com.igormaznitsa.prologparser.exceptions;

public class PrologParserException extends RuntimeException {
  private static final long serialVersionUID = -4404323844125857006L;

  protected final int lineNumber;
  protected final int stringPosition;

  public PrologParserException(final String text, final int lineNumber, final int stringPos) {
    this(text, lineNumber, stringPos, null);
  }

  public PrologParserException(final String text, final int lineNumber, final int stringPos, final Throwable cause) {
    super(text, cause);
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
