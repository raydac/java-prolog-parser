package com.igormaznitsa.prologparser.operators;

import java.util.Optional;
import java.util.stream.Stream;

public enum OpType {

  XF("xf", 1),
  YF("yf", 1),
  FX("fx", 1),
  FY("fy", 1),
  XFX("xfx", 2),
  XFY("xfy", 2),
  YFX("yfx", 2);

  private final String text;

  private final int arity;

  OpType(final String text, final int arity) {
    this.text = text;
    this.arity = arity;
  }

  public static Optional<OpType> getForName(final String str) {
    return Stream.of(values()).filter(x -> x.text.equals(str)).findFirst();
  }

  public int getArity() {
    return this.arity;
  }

  public String getText() {
    return text;
  }
}
