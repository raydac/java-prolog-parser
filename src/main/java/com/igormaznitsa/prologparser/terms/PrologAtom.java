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

import com.igormaznitsa.prologparser.utils.StringBuilderEx;
import com.igormaznitsa.prologparser.utils.StringUtils;

public final class PrologAtom extends PrologTerm {

  private static final long serialVersionUID = -1859006002358498466L;

  public PrologAtom(final String text) {
    super(text);
  }

  public PrologAtom(final PrologTerm term) {
    super(term.getText(), term.getLine(), term.getPos());
  }

  public PrologAtom(final String text, final int line, final int pos) {
    super(text, line, pos);
  }

  @Override
  public TermType getType() {
    return TermType.ATOM;
  }

  @Override
  public int getPrecedence() {
    return 0;
  }

  @Override
  public String toString() {
    final String escaped = StringUtils.escapeString(text);
    return new StringBuilderEx(escaped.length() + 2).append('\'').append(escaped).append('\'').toString();
  }
}
