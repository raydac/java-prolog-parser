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
 * Representation of prolog variable.
 */
@SuppressWarnings("unused")
public final class PrologVar extends PrologTerm {
  private static final long serialVersionUID = 1158349084517573220L;
  private final boolean anonymous;

  /**
   * Constructor of anonymous variable.
   */
  public PrologVar() {
    this("_");
  }

  public PrologVar(final int line, final int pos) {
    this();
    setPos(pos);
    setLine(line);
  }

  public PrologVar(final String text) {
    super(text, Quotation.NONE);

    final char startWith = assertNonEmptyString(text).charAt(0);

    if (!Character.isUpperCase(startWith) && startWith != '_') {
      throw new IllegalArgumentException(
          "Var must start with upper case char or '_' [" + text + ']');
    }

    this.anonymous = text.length() == 1 && startWith == '_';
  }

  public PrologVar(final String text, final int line, final int pos) {
    this(text);
    setPos(pos);
    setLine(line);
  }

  /**
   * Check that the variable anonymous one.
   *
   * @return true if the variable is anonymous one, false otherwise
   */
  public boolean isAnonymous() {
    return anonymous;
  }

  @Override
  public TermType getType() {
    return TermType.VAR;
  }
}
