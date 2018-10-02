package com.igormaznitsa.prologparser.operators;

import java.util.Optional;

import static java.util.Arrays.stream;

public enum OperatorType {

  XF("xf", 1),
  YF("yf", 1),
  FX("fx", 1),
  FY("fy", 1),
  XFX("xfx", 2),
  XFY("xfy", 2),
  YFX("yfx", 2);

  private final String text;

  private final int arity;

  OperatorType(final String text, final int arity) {
    this.text = text;
    this.arity = arity;
  }

  public static Optional<OperatorType> getForName(final String str) {
    return stream(values()).filter(x -> x.text.equals(str)).findFirst();
  }

  public int getArity() {
    return this.arity;
  }

  public String getText() {
    return text;
  }
}
