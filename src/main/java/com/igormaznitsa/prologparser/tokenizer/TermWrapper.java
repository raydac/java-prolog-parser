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

package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;

final class TermWrapper extends PrologTerm {

  private static final long serialVersionUID = 9006607815982718325L;
  private final SoftObjectPool<TermWrapper> pool;
  private PrologTerm term;

  TermWrapper(final SoftObjectPool<TermWrapper> pool) {
    super("termWrapper");
    this.pool = pool;
  }

  void release() {
    this.term = null;
    this.pool.push(this);
  }

  PrologTerm getTerm() {
    return this.term;
  }

  TermWrapper setTerm(final PrologTerm term) {
    this.term = term;
    return this;
  }

  @Override
  public TermType getTermType() {
    return term.getTermType();
  }

  @Override
  public String getTermText() {
    return term.getTermText();
  }

  @Override
  public int getPrecedence() {
    return term.getPrecedence();
  }

  @Override
  public String toString() {
    return term.toString();
  }

}
