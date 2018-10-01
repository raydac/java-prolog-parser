/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.igormaznitsa.prologparser.utils;

public final class StrBuffer {

  private final StringBuilder stringBuilder;

  public StrBuffer(final String initialString) {
    this.stringBuilder = new StringBuilder(initialString);
  }

  public StrBuffer(final int capacity) {
    this.stringBuilder = new StringBuilder(capacity);
  }

  public StrBuffer append(final char chr) {
    this.stringBuilder.append(chr);
    return this;
  }

  public int length() {
    return this.stringBuilder.length();
  }

  public char charAt(final int position) {
    return this.stringBuilder.charAt(position);
  }

  public int lastIndexOf(final String str) {
    return this.stringBuilder.lastIndexOf(str);
  }

  public boolean hasSingleChar(final char chr) {
    return this.stringBuilder.length() == 1 && this.stringBuilder.charAt(0) == chr;
  }

  public boolean isLastChar(final char chr) {
    final int length = this.stringBuilder.length();
    return length > 0 && this.stringBuilder.charAt(length - 1) == chr;
  }

  public String toStringExcludeLastChar() {
    return this.stringBuilder.substring(0, this.stringBuilder.length() - 1);
  }

  public StrBuffer append(final String str) {
    this.stringBuilder.append(str);
    return this;
  }

  public void clear() {
    this.stringBuilder.setLength(0);
  }

  @Override
  public String toString() {
    return this.stringBuilder.toString();
  }

  public void pushChar(final char chr) {
    this.stringBuilder.append(chr);
  }

  public char popChar() {
    final char chr = this.stringBuilder.charAt(this.stringBuilder.length() - 1);
    this.stringBuilder.setLength(this.stringBuilder.length() - 1);
    return chr;
  }

  public boolean isEmpty() {
    return this.stringBuilder.length() == 0;
  }

}
