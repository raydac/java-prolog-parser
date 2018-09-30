/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.igormaznitsa.prologparser.terms;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

/**
 * The class describes an immutable integer numeric atom, it is like a prolog
 * atom but being used to save a Java BigInteger value.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @see BigInteger
 */
public final class PrologIntegerNumber extends AbstractPrologNumericTerm {

  private static final long serialVersionUID = 6955747225309951985L;
  /**
   * Inside cache for text-number pairs
   */
  private static final Map<String, BigInteger> cachedValues = new HashMap<String, BigInteger>(128);

  static {
    // cache numbers from -99 to 99
    for (int i = -99; i < 100; i++) {
      cachedValues.put(Integer.toString(i), BigInteger.valueOf((long) i));
    }
  }

  /**
   * The variable contains immutable numeric value for the instance.
   */
  private final BigInteger value;

  /**
   * A Constructor. It allows to create new instance based on a text numeric
   * representation compatibles with BigInteger object.
   *
   * @param text the text represents a BigInteger value, must not be null
   */
  public PrologIntegerNumber(final String text) {
    super();
    value = valueOf(text);
  }

  /**
   * A Constructor. It allows to create new instance based on a text
   * compatibles with Java BigInteger class
   *
   * @param text    the text represents numeric value in BigInteger compatible
   *                manner, must not be null
   * @param strPos  the first term char string position
   * @param lineNum the first term char line number
   * @see BigInteger
   */
  public PrologIntegerNumber(final String text, final int strPos, final int lineNum) {
    this(valueOf(text), strPos, lineNum);
  }

  /**
   * A Constructor. It allows to create new instances based on a Java long
   * number. value.
   *
   * @param value a long value to make new instance.
   */
  public PrologIntegerNumber(final long value) {
    this(BigInteger.valueOf(value));
  }

  /**
   * A Constructor. It allows to make new instances based on a Java long
   * number.
   *
   * @param value      a long value to make new instance.
   * @param strPos     the first term char string position
   * @param lineNumber the first term char line number
   */
  public PrologIntegerNumber(final long value, final int strPos, final int lineNumber) {
    this(BigInteger.valueOf(value), strPos, lineNumber);
  }

  /**
   * A Constructor. It allows to create new instance based on a Java
   * BigInteger value.
   *
   * @param value the value to be saved in the new instance, must not be null
   */
  public PrologIntegerNumber(final BigInteger value) {
    super();
    if (value == null) {
      throw new NullPointerException("Value is null");
    }
    this.value = value;
  }

  /**
   * A Constructor. It allows to create new instance based on a Java
   * BigInteger value.
   *
   * @param value       the value to be saved into the new instance, must not be
   *                    null.
   * @param strPosition the first term char string position
   * @param lineNumber  the first term char line number
   */
  public PrologIntegerNumber(final BigInteger value, final int strPosition, final int lineNumber) {
    this(value);
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  /**
   * Find in cache a number for a text or to create a new on if not found.
   *
   * @param text a text to be converted into BigInteger, it must not be null
   * @return the numeric value of the text as a BigInteger
   */
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

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractPrologNumericTerm neg() {
    return new PrologIntegerNumber(value.negate());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return value.toString();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return toString();
  }

  /**
   * Get the immutable value saved by the instance
   *
   * @return the value as BigInteger
   */
  public BigInteger getValue() {
    return value;
  }
}
