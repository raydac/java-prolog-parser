package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.AssertUtils;

import java.math.BigDecimal;
import java.math.MathContext;

public final class PrologFloat extends PrologNumeric {

  public static final MathContext MATH_CONTEXT = MathContext.DECIMAL64;
  private static final long serialVersionUID = -8375787317103140082L;
  private final BigDecimal value;

  public PrologFloat(final String text) {
    this(new BigDecimal(text, MATH_CONTEXT));
  }

  public PrologFloat(final String text, final int line, final int pos) {
    this(new BigDecimal(text, MATH_CONTEXT), line, pos);
  }

  public PrologFloat(final double value) {
    this(BigDecimal.valueOf(value));
  }

  public PrologFloat(final double value, final int line, final int pos) {
    this(BigDecimal.valueOf(value), line, pos);
  }

  public PrologFloat(final BigDecimal value) {
    super();
    this.value = AssertUtils.assertNotNull(value);
  }

  public PrologFloat(final BigDecimal value, final int line, final int pos) {
    super(line, pos);
    this.value = AssertUtils.assertNotNull(value);
  }

  @Override
  public PrologNumeric neg() {
    return new PrologFloat(value.negate());
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