package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.AssertUtils;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.IntStream;

public final class PrologIntegerNumber extends PrologNumericTerm {

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

  public PrologIntegerNumber(final String text, final int line, final int pos) {
    this(valueOf(text), line, pos);
  }

  public PrologIntegerNumber(final long value) {
    this(BigInteger.valueOf(value));
  }

  public PrologIntegerNumber(final long value, final int line, final int pos) {
    this(BigInteger.valueOf(value), line, pos);
  }

  public PrologIntegerNumber(final BigInteger value) {
    super();
    this.value = AssertUtils.assertNotNull(value);
  }

  public PrologIntegerNumber(final BigInteger value, final int line, final int pos) {
    super(line, pos);
    this.value = AssertUtils.assertNotNull(value);
  }

  private static BigInteger valueOf(final String text) {
    AssertUtils.assertStringNotNullAndNotEmpty(text);
    BigInteger result = null;
    if (text.charAt(0) == '-') {
      if (text.length() < 4) {
        result = cachedValues.get(text);
      }
    } else if (text.length() < 3) {
      result = cachedValues.get(text);
    }

    if (result == null) {
      result = new BigInteger(text, 10);
    }
    return result;
  }

  @Override
  public PrologNumericTerm neg() {
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
