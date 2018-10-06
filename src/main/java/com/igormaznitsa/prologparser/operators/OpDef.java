package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.utils.AssertUtils;

import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.unmodifiableList;

public final class OpDef {

  private final int precedence;
  private final OpType type;
  private final List<String> names;

  private OpDef(final int precedence, final OpType type, final String... names) {
    AssertUtils.assertNotNull(type);
    if (precedence < 0 || precedence > 1200) {
      throw new IllegalArgumentException("Precedence must be in 0..1200");
    }
    this.precedence = precedence;
    this.type = type;
    this.names = unmodifiableList(asList(names));
  }

  public static OpDef op(final int precedence, final OpType type, final String... names) {
    return new OpDef(precedence, type, names);
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
