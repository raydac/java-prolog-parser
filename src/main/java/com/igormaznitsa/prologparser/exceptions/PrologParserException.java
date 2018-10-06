package com.igormaznitsa.prologparser.exceptions;

public class PrologParserException extends RuntimeException {
  private static final long serialVersionUID = -4404323844125857006L;

  protected final int line;
  protected final int pos;

  public PrologParserException(final String text, final int line, final int pos) {
    this(text, line, pos, null);
  }

  public PrologParserException(final String text, final int line, final int pos, final Throwable cause) {
    super(text, cause);
    this.line = line;
    this.pos = pos;
  }

  public int getLine() {
    return this.line;
  }

  public int getPos() {
    return this.pos;
  }

  public boolean hasValidPosition() {
    return this.line > 0 && this.pos > 0;
  }

  @Override
  public String toString() {
    return String.format("%s[%d:%d]", this.getMessage(), this.line, this.pos);
  }
}
