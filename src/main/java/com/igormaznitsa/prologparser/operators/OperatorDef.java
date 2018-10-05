package com.igormaznitsa.prologparser.operators;

public final class OperatorDef {

  private final int precedence;
  private final OpType type;
  private final String[] names;

  private OperatorDef(final int precedence, final OpType type, final String... names) {
    if (precedence < 0 || precedence > 1200) {
      throw new IllegalArgumentException("Precedence must be in 0..1200");
    }
    if (type == null) {
      throw new NullPointerException("Type is null");
    }
    this.precedence = precedence;
    this.type = type;
    this.names = names.clone();
  }

  public static OperatorDef of(final int precedence, final OpType type, final String... names) {
    return new OperatorDef(precedence, type, names);
  }

  public int getPrecedence() {
    return this.precedence;
  }

  public String[] getNames() {
    return this.names.clone();
  }

  public OpType getType() {
    return this.type;
  }
}