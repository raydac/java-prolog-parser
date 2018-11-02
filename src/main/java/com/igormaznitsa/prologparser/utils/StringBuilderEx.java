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

package com.igormaznitsa.prologparser.utils;

import com.igormaznitsa.prologparser.exceptions.CharBufferOverflowException;

public final class StringBuilderEx {

  private final int limit;

  private final StringBuilder stringBuilder;

  public StringBuilderEx(final String initialString) {
    this(initialString, Integer.MAX_VALUE);
  }

  public StringBuilderEx(final int capacity) {
    this(capacity, Integer.MAX_VALUE);
  }

  public StringBuilderEx(final String initialString, final int limit) {
    this.stringBuilder = new StringBuilder(initialString);
    this.limit = assertLimitValue(limit);
    assertBufferLimit();
  }

  public StringBuilderEx(final int capacity, final int limit) {
    this.stringBuilder = new StringBuilder(capacity);
    this.limit = assertLimitValue(limit);
  }

  public StringBuilderEx append(final char chr) {
    this.stringBuilder.append(chr);
    assertBufferLimit();
    return this;
  }

  public int length() {
    return this.stringBuilder.length();
  }

  public char charAt(final int position) {
    return this.stringBuilder.charAt(position);
  }

  private int assertLimitValue(final int limit) {
    if (limit <= 0) {
      throw new IllegalArgumentException("Limit must be positive: " + limit);
    }
    return limit;
  }

  public int lastIndexOf(final char chr) {
    for (int i = this.stringBuilder.length() - 1; i >= 0; i--) {
      if (this.stringBuilder.charAt(i) == chr) {
        return i;
      }
    }
    return -1;
  }

  public boolean isSingleChar(final char chr) {
    return this.stringBuilder.length() == 1 && this.stringBuilder.charAt(0) == chr;
  }

  public boolean isLastChar(final char chr) {
    final int length = this.stringBuilder.length();
    return length > 0 && this.stringBuilder.charAt(length - 1) == chr;
  }

  public String toStringExcludeLastChar() {
    return this.stringBuilder.substring(0, this.stringBuilder.length() - 1);
  }

  public boolean hasSeveralChars() {
    return this.stringBuilder.length() > 1;
  }

  public char getLastChar() {
    return this.stringBuilder.charAt(this.stringBuilder.length() - 1);
  }

  public String substring(final int start) {
    return this.stringBuilder.substring(start);
  }

  public String substring(final int start, final int end) {
    return this.stringBuilder.substring(start, end);
  }

  public StringBuilderEx append(final String str) {
    this.stringBuilder.append(str);
    assertBufferLimit();
    return this;
  }

  public void clear() {
    this.stringBuilder.setLength(0);
  }

  @Override
  public String toString() {
    return this.stringBuilder.toString();
  }

  private void assertBufferLimit() {
    if (this.stringBuilder.length() > this.limit) {
      throw new CharBufferOverflowException(this.stringBuilder.toString());
    }
  }

  public void push(final char chr) {
    this.stringBuilder.append(chr);
    assertBufferLimit();
  }

  public char pop() {
    final char chr = this.stringBuilder.charAt(this.stringBuilder.length() - 1);
    this.stringBuilder.setLength(this.stringBuilder.length() - 1);
    return chr;
  }

  public boolean isEmpty() {
    return this.stringBuilder.length() == 0;
  }

  public boolean isFirstCharDigit() {
    return this.stringBuilder.length()>0 && Character.isDigit(this.stringBuilder.charAt(0));
  }
}
