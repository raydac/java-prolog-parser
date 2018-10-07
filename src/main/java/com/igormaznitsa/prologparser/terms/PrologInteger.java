/*
 * Copyright (c) 2011-2018 Igor Maznitsa. All rights reserved.
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.AssertUtils;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.IntStream;

public final class PrologInteger extends PrologNumeric {

  private static final long serialVersionUID = 6955747225309951985L;
  private static final Map<String, BigInteger> cachedValues = new HashMap<>(128);

  static {
    IntStream.range(-99, 100).forEach(x -> cachedValues.put(Integer.toString(x), BigInteger.valueOf((long) x)));
  }

  private final BigInteger value;

  public PrologInteger(final String text) {
    super();
    value = valueOf(text);
  }

  public PrologInteger(final String text, final int line, final int pos) {
    this(valueOf(text), line, pos);
  }

  public PrologInteger(final long value) {
    this(BigInteger.valueOf(value));
  }

  public PrologInteger(final long value, final int line, final int pos) {
    this(BigInteger.valueOf(value), line, pos);
  }

  public PrologInteger(final BigInteger value) {
    super();
    this.value = AssertUtils.assertNotNull(value);
  }

  public PrologInteger(final BigInteger value, final int line, final int pos) {
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
  public PrologNumeric neg() {
    return new PrologInteger(value.negate());
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
