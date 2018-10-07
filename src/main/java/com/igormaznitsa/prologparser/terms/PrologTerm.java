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

import java.io.Serializable;
import java.util.stream.Stream;

import static com.igormaznitsa.prologparser.utils.AssertUtils.assertNotNull;

public abstract class PrologTerm implements Serializable {

  private static final long serialVersionUID = 1402429096900255841L;

  protected final String text;
  private volatile Serializable payload;
  private int line;
  private int pos;

  public PrologTerm(final String text) {
    this.text = assertNotNull(text);
    this.pos = -1;
    this.line = -1;
  }

  public PrologTerm(final String text, final int line, final int pos) {
    this(text);
    setLine(line);
    setPos(pos);
  }

  public final int getPos() {
    return pos;
  }

  public final void setPos(final int pos) {
    this.pos = pos <= 0 ? -1 : pos;
  }

  public final int getLine() {
    return line;
  }

  public final void setLine(final int line) {
    this.line = line <= 0 ? -1 : line;
  }

  public String getText() {
    return this.text;
  }

  public int getPrecedence() {
    return 0;
  }

  @Override
  public String toString() {
    return text;
  }

  public Serializable getPayload() {
    return this.payload;
  }

  public void setPayload(final Serializable obj) {
    this.payload = obj;
  }

  public abstract TermType getType();

  public Stream<PrologTerm> stream() {
    return Stream.of(this);
  }
}
