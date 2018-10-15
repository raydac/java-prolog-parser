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

/**
 * Representation of prolog variable.
 */
public final class PrologVariable extends PrologTerm {
  private static final long serialVersionUID = 1158349084517573220L;
  private final boolean anonymous;

  public PrologVariable() {
    this("_");
  }

  public PrologVariable(final int line, final int pos) {
    this();
    setPos(pos);
    setLine(line);
  }

  public PrologVariable(final String text) {
    super(text, QuotingType.NO_QUOTED);

    final char startWith = AssertUtils.assertStringNotNullAndNotEmpty(text).charAt(0);

    if (!Character.isUpperCase(startWith) && startWith != '_') {
      throw new IllegalArgumentException("Var must start with upper case char or '_' [" + text + ']');
    }

    this.anonymous = text.length() == 1 && startWith == '_';
  }

  public PrologVariable(final String text, final int line, final int pos) {
    this(text);
    setPos(pos);
    setLine(line);
  }

  public boolean isAnonymous() {
    return anonymous;
  }

  @Override
  public TermType getTermType() {
    return TermType.VAR;
  }
}
