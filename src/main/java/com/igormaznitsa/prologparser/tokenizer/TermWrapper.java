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

import com.igormaznitsa.prologparser.terms.InternalSpecialCompoundTerm;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.terms.TermType;

/**
 * Internal term wrapper to build AST.
 */
@SuppressWarnings("unused")
public final class TermWrapper extends InternalSpecialCompoundTerm {

  private static final long serialVersionUID = 9006607815982718325L;
  private final PrologTerm wrappedTerm;

  public TermWrapper(final PrologTerm term) {
    super(".wrapper");
    this.wrappedTerm = term;
  }

  @Override
  public int getArity() {
    return this.wrappedTerm.getArity();
  }

  public PrologTerm getWrappedTerm() {
    return this.wrappedTerm;
  }

  @Override
  public Quotation getQuotation() {
    return this.wrappedTerm.getQuotation();
  }

  @Override
  public TermType getType() {
    return this.wrappedTerm.getType();
  }

  @Override
  public String getText() {
    return this.wrappedTerm.getText();
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
