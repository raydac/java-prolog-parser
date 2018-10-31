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

import java.math.BigDecimal;
import java.math.MathContext;

/**
 * Representation for float numeric term.
 */
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

  public BigDecimal getFloatValue() {
    return this.value;
  }

  @Override
  public Number getNumber() {
    return this.value;
  }

  @Override
  public String toString() {
    final String result = this.value.toEngineeringString();
    return result.indexOf('.') < 0 ? result + ".0" : result;
  }

  @Override
  public boolean isNegative() {
    return this.value.signum() < 0;
  }
}
