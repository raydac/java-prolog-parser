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

import static java.util.Objects.requireNonNull;

import com.igormaznitsa.prologparser.terms.PrologTerm;

/**
 * Internal object representing tokenizer result.
 */
public final class TokenizerResult {

  private final TokenizerState parserState;
  private final PrologTerm resultTerm;
  private final String rawString;
  private final int pos;
  private final int line;

  TokenizerResult(
      final PrologTerm term,
      final TokenizerState parserState,
      final String rawString,
      final int line,
      final int pos
  ) {
    this.resultTerm = requireNonNull(term);
    this.parserState = requireNonNull(parserState);
    this.rawString = rawString;
    this.pos = pos;
    this.line = line;
  }

  @Override
  public int hashCode() {
    return this.rawString.hashCode();
  }

  public boolean equals(final Object that) {
    if (that == null) {
      return false;
    }
    if (that == this) {
      return true;
    }
    if (that instanceof TokenizerResult) {
      final TokenizerResult thatTokenizerResult = (TokenizerResult) that;
      return this.pos == thatTokenizerResult.pos && this.line == thatTokenizerResult.line
          && this.parserState == thatTokenizerResult.parserState
          && this.rawString.equals(((TokenizerResult) that).rawString);
    }
    return false;
  }

  public String getRawString() {
    return this.rawString;
  }

  public TokenizerState getTokenizerState() {
    return this.parserState;
  }

  public PrologTerm getResult() {
    return this.resultTerm;
  }

  public int getPos() {
    return this.pos;
  }

  public int getLine() {
    return this.line;
  }

  @Override
  public String toString() {
    return "TokenizerResult(" + this.resultTerm + ')';
  }
}
