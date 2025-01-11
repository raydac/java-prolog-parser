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

package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.terms.Quotation;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Random;

@SuppressWarnings("unused")
public final class PrologSourceKoi7Generator extends InputStream {

  private final Random rnd = new Random(123);
  private final int maxChars;
  private final boolean throwException;
  private final int maxClauses;
  private final boolean splitClauses;
  private final boolean everyElementOnNewLine;
  private int charCounter;
  private int generatedClauseCounter;
  private String clauseBuffer;
  private int clausePos;

  public PrologSourceKoi7Generator(
      final int maxClauses,
      final boolean splitClauses,
      final boolean everyElementOnNewLine
  ) {
    this(splitClauses, Integer.MAX_VALUE, maxClauses, everyElementOnNewLine, false);
  }

  public PrologSourceKoi7Generator(final boolean splitClauses,
                                   final int maxChars,
                                   final int maxClauses,
                                   final boolean everyElementOnNewLine,
                                   final boolean throwException) {
    this.generatedClauseCounter = 0;
    this.splitClauses = splitClauses;
    this.maxChars = maxChars;
    this.throwException = throwException;
    this.maxClauses = maxClauses;
    this.everyElementOnNewLine = everyElementOnNewLine;
    genNextClause();
  }

  public String asString() {
    final StringBuilder buffer = new StringBuilder();
    try (final InputStreamReader reader = new InputStreamReader(this)) {
      while (true) {
        final int next = reader.read();
        if (next < 0) {
          break;
        }
        buffer.append((char) next);
      }
    } catch (IOException ex) {
      throw new RuntimeException(ex);
    }
    return buffer.toString();
  }

  private void genNextClause() {
    this.clauseBuffer = this.generateClause();
    this.clausePos = 0;
    this.generatedClauseCounter++;
  }

  private String generateClause() {
    final StringBuilder builder = new StringBuilder();

    builder.append(this.generateOperator(this.rnd.nextInt(10) + 2));
    if (Character.isDigit(builder.charAt(builder.length() - 1))) {
      builder.append(". ");
    } else {
      builder.append('.');
    }

    if (this.splitClauses) {
      if (this.rnd.nextInt(100) > 50) {
        builder.append(' ');
        if (this.rnd.nextInt(100) > 80) {
          builder.append("% ").append(generateAtom()).append('\n');
        }
      } else {
        builder.append('\n');
      }
    }

    return builder.toString();
  }

  private String makeRndTerm(int recursionLevel) {
    final String result;
    switch (this.rnd.nextInt(8)) {
      case 0:
        result = generateNumber();
        break;
      case 1:
        result = generateString();
        break;
      case 2:
        result = generateAtom();
        break;
      case 3:
        result = generateVar();
        break;
      case 4:
        result = generateList(recursionLevel - 1);
        break;
      case 5:
        result = generateOperator(recursionLevel - 1);
        break;
      default: {
        result = generateStruct(recursionLevel - 1);
      }
      break;
    }
    return this.everyElementOnNewLine ? '\n' + result : result;
  }

  private String generateOperator(int recursionLevel) {
    switch (this.rnd.nextInt(10)) {
      case 0: {
        return this.makeRndTerm(recursionLevel - 1) + ',' + this.makeRndTerm(recursionLevel - 1);
      }
      case 1: {
        return this.makeRndTerm(recursionLevel - 1) + ';' + this.makeRndTerm(recursionLevel - 1);
      }
      case 2: {
        return this.makeRndTerm(recursionLevel - 1) + '/' + this.makeRndTerm(recursionLevel - 1);
      }
      case 3: {
        return this.makeRndTerm(recursionLevel - 1) + '+' + this.makeRndTerm(recursionLevel - 1);
      }
      case 4: {
        return this.makeRndTerm(recursionLevel - 1) + '-' + this.makeRndTerm(recursionLevel - 1);
      }
      case 5: {
        return this.makeRndTerm(recursionLevel - 1) + '^' + this.makeRndTerm(recursionLevel - 1);
      }
      case 6: {
        return this.makeRndTerm(recursionLevel - 1) + ':' + this.makeRndTerm(recursionLevel - 1);
      }
      case 7: {
        return this.makeRndTerm(recursionLevel - 1) + " mod " +
            this.makeRndTerm(recursionLevel - 1);
      }
      default: {
        return this.makeRndTerm(recursionLevel - 1) + " rem " +
            this.makeRndTerm(recursionLevel - 1);
      }
    }
  }

  private String generateStruct(final int recusionLevel) {
    if (recusionLevel <= 0) {
      return "''(123_345)";
    }

    final StringBuilder builder = new StringBuilder();

    if (this.rnd.nextInt(100) > 50) {
      builder.append(generateAtom());
    } else {
      builder.append(generateString());
    }

    builder.append('(');

    final int len = this.rnd.nextInt(32) + 1;

    for (int i = 0; i < len; i++) {
      if (i > 0) {
        builder.append(',');
      }
      builder.append(makeRndTerm(recusionLevel - 1));
    }

    builder.append(')');
    return builder.toString();
  }

  private String generateList(final int recusionLevel) {
    if (recusionLevel <= 0) {
      return "[]";
    }

    final StringBuilder builder = new StringBuilder();
    builder.append('[');

    boolean tailAdded = false;

    final int len = this.rnd.nextInt(16);
    for (int i = 0; i < len; i++) {
      if (i != 0) {
        if (i < len - 1 && !tailAdded && this.rnd.nextInt(10) > 8) {
          builder.append("|(");
          tailAdded = true;
        } else {
          builder.append(',');
        }
      }

      builder.append(makeRndTerm(recusionLevel - 1));
    }


    builder.append(tailAdded ? ")]" : "]");
    return builder.toString();
  }

  private String generateNumber() {
    final StringBuilder builder = new StringBuilder();
    final int len = this.rnd.nextInt(32) + 1;
    for (int i = 0; i < len; i++) {
      builder.append((char) ('0' + this.rnd.nextInt('9' - '0')));
    }
    return builder.toString();
  }

  private String generateString() {
    final Quotation selectedQuotation;
    switch (this.rnd.nextInt(3)) {
      case 0:
        selectedQuotation = Quotation.SINGLE;
        break;
      case 1:
        selectedQuotation = Quotation.BACK_TICK;
        break;
      default:
        selectedQuotation = Quotation.DOUBLE;
        break;
    }

    final StringBuilder buffer = new StringBuilder();

    final int generatedLength = this.rnd.nextInt(32) + 1;
    for (int i = 0; i < generatedLength; i++) {
      if (i == 0) {
        buffer.append((char) ('a' + this.rnd.nextInt('z' - 'a')));
      } else {
        switch (this.rnd.nextInt(8)) {
          case 0: {
            buffer.append((char) ('a' + this.rnd.nextInt('z' - 'a')));
          }
          break;
          case 1: {
            buffer.append((char) ('A' + this.rnd.nextInt('Z' - 'A')));
          }
          break;
          case 2: {
            buffer.append(' ');
          }
          break;
          case 3: {
            switch (this.rnd.nextInt(10)) {
              case 0:
                buffer.append("\\n");
                break;
              case 1:
                buffer.append("\\r");
                break;
              case 2:
                buffer.append("\\\"");
                break;
              case 3:
                buffer.append("\\`");
                break;
              case 4:
                buffer.append("\\'");
                break;
              case 5:
                buffer.append("\\f");
                break;
              case 6:
                buffer.append("\\t");
                break;
              default:
                buffer.append("\\b");
                break;
            }
          }
          break;
          case 4:
            buffer.append('.');
            break;
          case 5:
            buffer.append('|');
            break;
          case 6:
            buffer.append(',');
            break;
          case 7:
            buffer.append('-');
            break;
          default: {
            buffer.append((char) ('0' + this.rnd.nextInt('9' - '0')));
          }
          break;
        }
      }
    }

    final String text = buffer.toString();
    return selectedQuotation.formatString(text);
  }

  private String generateAtom() {
    final int len = this.rnd.nextInt(32) + 1;
    final StringBuilder buffer = new StringBuilder(len);

    boolean upper = false;
    for (int i = 0; i < len; i++) {
      if (upper) {
        buffer.append((char) ('A' + this.rnd.nextInt('Z' - 'A')));
      } else {
        buffer.append((char) ('a' + this.rnd.nextInt('z' - 'a')));
      }

      upper = !upper;
    }

    return buffer.toString();
  }

  private String generateVar() {
    final StringBuilder buffer = new StringBuilder();
    if (this.rnd.nextInt(100) > 90) {
      buffer.append("_");
    } else {
      buffer.append((char) ('A' + this.rnd.nextInt('Z' - 'A')));
    }
    final int num = this.rnd.nextInt(8);
    for (int i = 0; i < num; i++) {
      buffer.append((char) ('a' + this.rnd.nextInt('z' - 'a')));
    }
    return buffer.toString();
  }

  @Override
  public int read() throws IOException {
    if (this.generatedClauseCounter <= this.maxClauses && this.charCounter < this.maxChars) {
      if (this.clausePos == this.clauseBuffer.length()) {
        genNextClause();
      }
    }

    if (this.generatedClauseCounter <= this.maxClauses && this.charCounter < this.maxChars) {
      this.charCounter++;
      return this.clauseBuffer.charAt(this.clausePos++);
    } else {
      if (this.throwException) {
        throw new IOException("Exception on demand");
      } else {
        return -1;
      }
    }
  }

  public String makeString() throws IOException {
    final StringBuilder builder = new StringBuilder();
    while (!Thread.currentThread().isInterrupted()) {
      final int next = this.read();
      if (next < 0) {
        break;
      }
      builder.append((char) next);
    }

    this.generatedClauseCounter = 0;
    this.charCounter = 0;
    genNextClause();

    return builder.toString();
  }
}
