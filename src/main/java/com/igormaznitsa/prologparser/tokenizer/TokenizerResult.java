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
import com.igormaznitsa.prologparser.utils.AssertUtils;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;

/**
 * Internal object representing tokenizer result.
 */
public final class TokenizerResult {
  private final SoftObjectPool<TokenizerResult> pool;
  private TokenizerState parserState;
  private PrologTerm resultTerm;
  private int pos;
  private int line;

  TokenizerResult(final SoftObjectPool<TokenizerResult> pool) {
    this.pool = pool;
  }

  TokenizerResult setData(
      final PrologTerm term,
      final TokenizerState parserState,
      final int line,
      final int pos
  ) {
    this.resultTerm = AssertUtils.assertNotNull(term);
    this.parserState = AssertUtils.assertNotNull(parserState);
    this.pos = pos;
    this.line = line;
    return this;
  }

  public void release() {
    this.parserState = null;
    this.resultTerm = null;
    this.pos = -1;
    this.line = -1;
    this.pool.push(this);
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
