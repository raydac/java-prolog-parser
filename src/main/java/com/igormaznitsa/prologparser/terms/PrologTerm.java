package com.igormaznitsa.prologparser.terms;

import java.io.Serializable;

import static com.igormaznitsa.prologparser.utils.AssertUtils.assertNotNull;

public abstract class PrologTerm implements Serializable {

  private static final long serialVersionUID = 1402429096900255841L;

  protected final String text;
  private volatile Serializable payload;
  private int line;
  private int pos;

  public PrologTerm(final String text) {
    this.text = assertNotNull(text);
    this.pos = -1;
    this.line = -1;
  }

  public PrologTerm(final String text, final int line, final int pos) {
    this(text);
    setLine(line);
    setPos(pos);
  }

  public final int getPos() {
    return pos;
  }

  public final void setPos(final int pos) {
    this.pos = pos <= 0 ? -1 : pos;
  }

  public final int getLine() {
    return line;
  }

  public final void setLine(final int line) {
    this.line = line <= 0 ? -1 : line;
  }

  public String getText() {
    return this.text;
  }

  public int getPrecedence() {
    return 0;
  }

  @Override
  public String toString() {
    return text;
  }

  public Serializable getPayload() {
    return this.payload;
  }

  public void setPayload(final Serializable obj) {
    this.payload = obj;
  }

  public abstract TermType getType();
}
