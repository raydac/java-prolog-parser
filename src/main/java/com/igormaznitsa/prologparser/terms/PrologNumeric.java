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

/**
 * Base class for all numeric terms.
 */
public abstract class PrologNumeric extends PrologTerm {

  private static final long serialVersionUID = -1815562758090770438L;

  public PrologNumeric() {
    super("", Quotation.NONE);
  }

  public PrologNumeric(final int line, final int pos) {
    super("", Quotation.NONE, line, pos);
  }

  /**
   * Get numeric repreentation of the saved number.
   *
   * @return the saved number
   */
  public abstract Number getNumber();

  @Override
  public final TermType getType() {
    return TermType.ATOM;
  }

  @Override
  public String getText() {
    return toString();
  }

  /**
   * Check that the number is negative one
   *
   * @return true if the number is negative one, false otherwise
   */
  public abstract boolean isNegative();

  /**
   * Make negative representation of the numeric term
   *
   * @return the negative variant of the numeric
   */
  public abstract PrologNumeric makeNeg();
}
