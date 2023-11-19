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

import com.igormaznitsa.prologparser.terms.Quotation;

/**
 * Auxiliary string processing methods.
 */
public final class StringUtils {

  private StringUtils() {
  }

  /**
   * Unescape special char which definition is in the buffer.
   *
   * @param stringAfterEscMarker buffer contains string
   * @return result container, must not be null
   */
  public static UnescapeResult tryUnescapeCharacter(final StringBuilderEx stringAfterEscMarker) {
    if (stringAfterEscMarker == null || stringAfterEscMarker.isEmpty()) {
      return new UnescapeResult('_', false, true);
    }

    final int len = stringAfterEscMarker.length();

    final char result;

    if (len == 1) {
      switch (stringAfterEscMarker.charAt(0)) {
        case 'a':
          result = (char) 7;
          break;
        case 'b':
          result = '\b';
          break;
        case 'n':
          result = '\n';
          break;
        case 'f':
          result = '\f';
          break;
        case 'r':
          result = '\r';
          break;
        case 'e':
          result = (char) 27;
          break;
        case 't':
          result = '\t';
          break;
        case 's':
          result = (char) 32;
          break;
        case 'v':
          result = (char) 11;
          break;
        case '\\':
          result = '\\';
          break;
        case '\'':
          result = '\'';
          break;
        case '\"':
          result = '\"';
          break;
        case '`':
          result = '`';
          break;
        case 'u':
        case 'x':
          return new UnescapeResult(stringAfterEscMarker.charAt(0), true, false);
        default: {
          if (stringAfterEscMarker.isFirstCharDigit() && stringAfterEscMarker.charAt(0) < '8') {
            return new UnescapeResult(stringAfterEscMarker.charAt(0), true, false);
          } else {
            return new UnescapeResult(stringAfterEscMarker.charAt(0), false, true);
          }
        }
      }
      return new UnescapeResult(result, false, false);
    } else {
      switch (stringAfterEscMarker.charAt(0)) {
        case 'u': {
          if (len == 5) {
            final int decoded;
            try {
              decoded = Integer.parseInt(stringAfterEscMarker.substring(1), 16);
            } catch (NumberFormatException ex) {
              return new UnescapeResult('u', false, true);
            }
            return new UnescapeResult((char) decoded, false, false);
          } else {
            if (len > 5) {
              return new UnescapeResult('u', false, true);
            } else {
              if (stringAfterEscMarker.hasSeveralChars() &&
                  isCharNotAppropriateForHexNum(stringAfterEscMarker.getLastChar())) {
                return new UnescapeResult('u', false, true);
              }
              return new UnescapeResult('u', true, false);
            }
          }
        }
        case 'x': {
          if (stringAfterEscMarker.isLastChar('\\')) {
            final int decoded;
            try {
              decoded = Integer.parseInt(
                  stringAfterEscMarker.substring(1, stringAfterEscMarker.length() - 1), 16);
            } catch (NumberFormatException ex) {
              return new UnescapeResult('x', false, true);
            }
            return new UnescapeResult((char) decoded, false, false);
          } else {
            if (stringAfterEscMarker.hasSeveralChars() &&
                isCharNotAppropriateForHexNum(stringAfterEscMarker.getLastChar())) {
              return new UnescapeResult('x', false, true);
            }
            return new UnescapeResult('x', true, false);
          }
        }
        default: {
          if (stringAfterEscMarker.isFirstCharDigit()) {
            if (stringAfterEscMarker.isLastChar('\\')) {
              final String charOctCode = stringAfterEscMarker.toStringExcludeLastChar();
              try {
                return new UnescapeResult((char) Integer.parseInt(charOctCode, 8), false, false);
              } catch (NumberFormatException ex) {
                return new UnescapeResult('/', false, true);
              }
            } else {
              return new UnescapeResult('/', true, false);
            }
          }
          return new UnescapeResult(stringAfterEscMarker.charAt(0), false, true);
        }
      }
    }
  }

  public static boolean isCharAllowedForUnquotedAtom(final char chr) {
    return Character.isLetterOrDigit(chr) || chr == '_';
  }

  public static boolean isCharNotAppropriateForHexNum(final char chr) {
    return (chr < '0' || chr > '9') && (chr < 'a' || chr > 'f') && (chr < 'A' || chr > 'F');
  }

  public static String escapeString(final String str, final Quotation quotingType) {
    final StringBuilder result = new StringBuilder(str.length() << 1);

    final int strLen = str.length();
    for (int i = 0; i < strLen; i++) {
      final char chr = str.charAt(i);
      switch (chr) {
        case 7:
          result.append("\\a");
          break;
        case 8:
          result.append("\\b");
          break;
        case '\f':
          result.append("\\f");
          break;
        case '\n':
          result.append("\\n");
          break;
        case '\r':
          result.append("\\r");
          break;
        case '`':
          if (quotingType == Quotation.BACK_TICK) {
            result.append("\\`");
          } else {
            result.append('`');
          }
          break;
        case 27:
          result.append("\\e");
          break;
        case '\t':
          result.append("\\t");
          break;
        case '\"':
          if (quotingType == Quotation.DOUBLE) {
            result.append("\\\"");
          } else {
            result.append('\"');
          }
          break;
        case '\'':
          if (quotingType == Quotation.SINGLE) {
            result.append("\\'");
          } else {
            result.append('\'');
          }
          break;
        case 11:
          result.append("\\v");
          break;
        default:
          if (Character.isISOControl(chr)) {
            result.append('\\').append(Integer.toOctalString(chr)).append('\\');
          } else {
            result.append(chr);
          }
          break;
      }
    }

    return result.toString();
  }

  /**
   * Un-escaped result container.
   */
  public static final class UnescapeResult {

    private final boolean needsMore;
    private final boolean error;
    private final char decoded;

    private UnescapeResult(final char decoded, final boolean needsMore, final boolean error) {
      this.decoded = decoded;
      this.needsMore = needsMore;
      this.error = error;
    }

    public boolean doesNeedMore() {
      return this.needsMore;
    }

    public boolean isError() {
      return this.error;
    }

    public char getDecoded() {
      return this.decoded;
    }
  }
}
