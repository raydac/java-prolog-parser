package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.AssertUtils;

import java.math.BigDecimal;
import java.math.MathContext;

public final class PrologFloatNumber extends PrologNumericTerm {

  public static final MathContext MATH_CONTEXT = MathContext.DECIMAL64;
  private static final long serialVersionUID = -8375787317103140082L;
  private final BigDecimal value;

  public PrologFloatNumber(final String text) {
    this(new BigDecimal(text, MATH_CONTEXT));
  }

  public PrologFloatNumber(final String text, final int line, final int pos) {
    this(new BigDecimal(text, MATH_CONTEXT), line, pos);
  }

  public PrologFloatNumber(final double value) {
    this(BigDecimal.valueOf(value));
  }

  public PrologFloatNumber(final double value, final int line, final int pos) {
    this(BigDecimal.valueOf(value), line, pos);
  }

  public PrologFloatNumber(final BigDecimal value) {
    super();
    this.value = AssertUtils.assertNotNull(value);
  }

  public PrologFloatNumber(final BigDecimal value, final int line, final int pos) {
    super(line, pos);
    this.value = AssertUtils.assertNotNull(value);
  }

  @Override
  public PrologNumericTerm neg() {
    return new PrologFloatNumber(value.negate());
  }

  public BigDecimal getValue() {
    return this.value;
  }

  @Override
  public String toString() {
    final String result = this.value.toEngineeringString();
    return result.indexOf('.') < 0 ? result + ".0" : result;
  }
}
