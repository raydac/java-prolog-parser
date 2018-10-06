package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.AssertUtils;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.IntStream;

public final class PrologIntegerNumber extends AbstractPrologNumericTerm {

  private static final long serialVersionUID = 6955747225309951985L;
  private static final Map<String, BigInteger> cachedValues = new HashMap<>(128);

  static {
    IntStream.range(-99, 100).forEach(x -> cachedValues.put(Integer.toString(x), BigInteger.valueOf((long) x)));
  }

  private final BigInteger value;

  public PrologIntegerNumber(final String text) {
    super();
    value = valueOf(text);
  }

  public PrologIntegerNumber(final String text, final int strPos, final int lineNum) {
    this(valueOf(text), strPos, lineNum);
  }

  public PrologIntegerNumber(final long value) {
    this(BigInteger.valueOf(value));
  }

  public PrologIntegerNumber(final long value, final int strPos, final int lineNumber) {
    this(BigInteger.valueOf(value), strPos, lineNumber);
  }

  public PrologIntegerNumber(final BigInteger value) {
    super();
    this.value = AssertUtils.assertNotNull(value);
  }

  public PrologIntegerNumber(final BigInteger value, final int strPosition, final int lineNumber) {
    super(strPosition, lineNumber);
    this.value = AssertUtils.assertNotNull(value);
  }

  private static BigInteger valueOf(final String text) {
    final int len = text.length();
    BigInteger result = null;
    if (len == 0) {
      throw new NumberFormatException("Empty string");
    } else {
      if (text.charAt(0) == '-') {
        if (len < 4) {
          result = cachedValues.get(text);
        }
      } else if (len < 3) {
        result = cachedValues.get(text);
      }

      if (result == null) {
        result = new BigInteger(text, 10);
      }
    }
    return result;
  }

  @Override
  public AbstractPrologNumericTerm neg() {
    return new PrologIntegerNumber(value.negate());
  }

  @Override
  public String toString() {
    return value.toString();
  }

  @Override
  public String getText() {
    return toString();
  }

  public BigInteger getValue() {
    return value;
  }
}
