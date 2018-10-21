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

import com.igormaznitsa.prologparser.terms.PrologTerm;

import java.io.IOException;
import java.io.InputStream;
import java.util.Random;

public final class PrologSourceKoi7Generator extends InputStream {

  private final Random rnd = new Random(123);
  private final int maxChars;
  private final boolean throwException;
  private final int maxSentences;
  private final boolean separateSentences;
  private int charCounter;
  private int generatedSentencesCounter;
  private String sentenceBuffer;
  private int sentencePos;

  public PrologSourceKoi7Generator(final int maxSentences, final boolean separateSentences) {
    this.generatedSentencesCounter = 0;
    this.separateSentences = separateSentences;
    this.maxChars = Integer.MAX_VALUE;
    this.throwException = false;
    this.maxSentences = maxSentences;
    genNextSentence();
  }

  public PrologSourceKoi7Generator(final boolean separateSentences, final int numberOfChars, final boolean throwException) {
    this.generatedSentencesCounter = 0;
    this.separateSentences = separateSentences;
    this.maxChars = numberOfChars;
    this.throwException = throwException;
    this.maxSentences = Integer.MAX_VALUE;
    genNextSentence();
  }

  private void genNextSentence() {
    this.sentenceBuffer = generateSentence();
    this.sentencePos = 0;
    this.generatedSentencesCounter++;
  }

  private String generateSentence() {
    final StringBuilder builder = new StringBuilder();

    builder.append(generateOperator(this.rnd.nextInt(10) + 2));
    if (Character.isDigit(builder.charAt(builder.length() - 1))) {
      builder.append(". ");
    } else {
      builder.append('.');
    }

    if (this.separateSentences) {
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

  private String makeRndTerm(int recusionLevel) {
    switch (this.rnd.nextInt(8)) {
      case 0:
        return generateNumber();
      case 1:
        return generateString();
      case 2:
        return generateAtom();
      case 3:
        return generateVar();
      case 4:
        return generateList(recusionLevel - 1);
      case 5:
        return generateOperator(recusionLevel - 1);
      default: {
        return generateStruct(recusionLevel - 1);
      }
    }
  }

  private String generateOperator(int recusionLevel) {
    switch (this.rnd.nextInt(10)) {
      case 0: {
        return makeRndTerm(recusionLevel - 1) + ',' + makeRndTerm(recusionLevel - 1);
      }
      case 1: {
        return makeRndTerm(recusionLevel - 1) + ';' + makeRndTerm(recusionLevel - 1);
      }
      case 2: {
        return makeRndTerm(recusionLevel - 1) + '/' + makeRndTerm(recusionLevel - 1);
      }
      case 3: {
        return makeRndTerm(recusionLevel - 1) + '+' + makeRndTerm(recusionLevel - 1);
      }
      case 4: {
        return makeRndTerm(recusionLevel - 1) + '-' + makeRndTerm(recusionLevel - 1);
      }
      case 5: {
        return makeRndTerm(recusionLevel - 1) + '^' + makeRndTerm(recusionLevel - 1);
      }
      case 6: {
        return makeRndTerm(recusionLevel - 1) + ':' + makeRndTerm(recusionLevel - 1);
      }
      case 7: {
        return makeRndTerm(recusionLevel - 1) + " mod " + makeRndTerm(recusionLevel - 1);
      }
      default: {
        return makeRndTerm(recusionLevel - 1) + " rem " + makeRndTerm(recusionLevel - 1);
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
          builder.append('|');
          tailAdded = true;
        } else {
          builder.append(',');
        }
      }

      builder.append(makeRndTerm(recusionLevel - 1));
    }


    builder.append(']');
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
    final PrologTerm.QuotingType type;
    switch (this.rnd.nextInt(3)) {
      case 0:
        type = PrologTerm.QuotingType.SINGLE_QUOTED;
        break;
      case 1:
        type = PrologTerm.QuotingType.BACK_QUOTED;
        break;
      default:
        type = PrologTerm.QuotingType.DOUBLE_QUOTED;
        break;
    }

    final int len = this.rnd.nextInt(32) + 1;
    final StringBuilder buffer = new StringBuilder(type.getDelimiter());

    for (int i = 0; i < len; i++) {
      if (i == 0) {
        buffer.append((char) ('a' + this.rnd.nextInt('z' - 'a')));
      } else {
        switch (this.rnd.nextInt(5)) {
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

    buffer.append(type.getDelimiter());

    return buffer.toString();
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
    if (this.generatedSentencesCounter <= this.maxSentences && this.charCounter < this.maxChars) {
      if (this.sentencePos == this.sentenceBuffer.length()) {
        genNextSentence();
      }
    }

    if (this.generatedSentencesCounter <= this.maxSentences && this.charCounter < this.maxChars) {
      this.charCounter++;
      return this.sentenceBuffer.charAt(this.sentencePos++);
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

    this.generatedSentencesCounter = 0;
    this.charCounter = 0;
    genNextSentence();

    return builder.toString();
  }
}
