package com.igormaznitsa.prologparser.terms;

import java.io.Serializable;

public abstract class AbstractPrologTerm implements Serializable {

  private static final long serialVersionUID = 1482429096900255841L;

  protected final String text;
  protected Object linkedObject;
  private int lineNumber;
  private int strPosition;

  public AbstractPrologTerm(final String text) {
    if (text == null) {
      throw new NullPointerException("Term text must not be null");
    }
    this.text = text;
    this.strPosition = -1;
    this.lineNumber = -1;
  }

  public AbstractPrologTerm(final String text, final int strPosition, final int lineNumber) {
    this(text);
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  public final int getStrPosition() {
    return strPosition;
  }

  public final void setStrPosition(final int strPosition) {
    this.strPosition = strPosition <= 0 ? -1 : strPosition;
  }

  public final int getLineNumber() {
    return lineNumber;
  }

  public final void setLineNumber(final int lineNumber) {
    this.lineNumber = lineNumber <= 0 ? -1 : lineNumber;
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

  public Object getLinkedObject() {
    return this.linkedObject;
  }

  public void setLinkedObject(final Object obj) {
    this.linkedObject = obj;
  }

  public abstract PrologTermType getType();
}
