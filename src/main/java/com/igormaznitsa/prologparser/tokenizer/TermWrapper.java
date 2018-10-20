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

import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.SpecServiceCompound;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;

import java.beans.Transient;

final class TermWrapper extends SpecServiceCompound {

  private static final long serialVersionUID = 9006607815982718325L;
  private transient final SoftObjectPool<TermWrapper> pool;
  private PrologTerm wrappedTerm;

  TermWrapper(final SoftObjectPool<TermWrapper> pool) {
    super(".wrapper");
    this.pool = pool;
  }

  void release() {
    this.wrappedTerm = null;
    this.pool.push(this);
  }

  @Override
  public int getArity() {
    return 1;
  }

  PrologTerm getWrappedTerm() {
    return this.wrappedTerm;
  }

  TermWrapper setWrappedTerm(final PrologTerm wrappedTerm) {
    this.wrappedTerm = wrappedTerm;
    return this;
  }

  public PrologAtom.QuotingType getQuotingType() {
    return this.wrappedTerm.getQuotingType();
  }

  @Override
  public TermType getTermType() {
    return this.wrappedTerm.getTermType();
  }

  @Override
  public String getTermText() {
    return this.wrappedTerm.getTermText();
  }

  @Override
  public int getPrecedence() {
    return this.wrappedTerm.getPrecedence();
  }

  @Override
  public String toString() {
    return this.wrappedTerm.toString();
  }

}
