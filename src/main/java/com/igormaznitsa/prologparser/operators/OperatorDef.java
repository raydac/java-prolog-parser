package com.igormaznitsa.prologparser.operators;

import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.unmodifiableList;

public final class OperatorDef {

  private final int precedence;
  private final OpType type;
  private final List<String> names;

  private OperatorDef(final int precedence, final OpType type, final String... names) {
    if (type == null) {
      throw new NullPointerException("Type is null");
    }
    if (precedence < 0 || precedence > 1200) {
      throw new IllegalArgumentException("Precedence must be in 0..1200");
    }
    this.precedence = precedence;
    this.type = type;
    this.names = unmodifiableList(asList(names));
  }

  public static OperatorDef of(final int precedence, final OpType type, final String... names) {
    return new OperatorDef(precedence, type, names);
  }

  public int getPrecedence() {
    return this.precedence;
  }

  public List<String> getNames() {
    return this.names;
  }

  public OpType getType() {
    return this.type;
  }
}
