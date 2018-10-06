package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.AssertUtils;

import java.math.BigDecimal;
import java.math.MathContext;

public final class PrologFloatNumber extends AbstractPrologNumericTerm {

  public static final MathContext MATH_CONTEXT = MathContext.DECIMAL64;
  private static final long serialVersionUID = -8375787317103540082L;
  private final BigDecimal value;

  public PrologFloatNumber(final String text) {
    this(new BigDecimal(text, MATH_CONTEXT));
  }

  public PrologFloatNumber(final String text, final int strPos, final int lineNumber) {
    this(new BigDecimal(text, MATH_CONTEXT), strPos, lineNumber);
  }

  public PrologFloatNumber(final double value) {
    this(BigDecimal.valueOf(value));
  }

  public PrologFloatNumber(final double value, final int strPosition, final int lineNumber) {
    this(BigDecimal.valueOf(value), strPosition, lineNumber);
  }

  public PrologFloatNumber(final BigDecimal value) {
    super();
    this.value = AssertUtils.assertNotNull(value);
  }

  public PrologFloatNumber(final BigDecimal value, final int strPosition, final int lineNumber) {
    super(strPosition, lineNumber);
    this.value = AssertUtils.assertNotNull(value);
  }

  @Override
  public AbstractPrologNumericTerm neg() {
    return new PrologFloatNumber(value.negate());
  }

  public BigDecimal getValue() {
    return value;
  }

  @Override
  public String toString() {
    final String result = value.toEngineeringString();
    return result.indexOf('.') < 0 ? result + ".0" : result;
  }
}
