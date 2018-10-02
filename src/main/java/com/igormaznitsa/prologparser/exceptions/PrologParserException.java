package com.igormaznitsa.prologparser.exceptions;

import java.io.IOException;

public class PrologParserException extends IOException {
  private static final long serialVersionUID = -4404323844625857006L;

  protected final int lineNumber;
  protected final int stringPosition;

  public PrologParserException(final String text, final int lineNumber, final int stringPos) {
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
