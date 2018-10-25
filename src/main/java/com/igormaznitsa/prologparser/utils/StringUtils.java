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

import com.igormaznitsa.prologparser.terms.PrologTerm;

import java.util.Locale;

@SuppressWarnings("serial")
public final class StringUtils {

  private StringUtils() {
  }

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
        default:
          return new UnescapeResult(stringAfterEscMarker.charAt(0), false, true);
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
              if (stringAfterEscMarker.hasSeveralChars() && isCharNotAppropriateForHex(stringAfterEscMarker.getLastChar())) {
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
              decoded = Integer.parseInt(stringAfterEscMarker.substring(1, stringAfterEscMarker.length() - 1), 16);
            } catch (NumberFormatException ex) {
              return new UnescapeResult('x', false, true);
            }
            return new UnescapeResult((char) decoded, false, false);
          } else {
            if (stringAfterEscMarker.hasSeveralChars() && isCharNotAppropriateForHex(stringAfterEscMarker.getLastChar())) {
              return new UnescapeResult('x', false, true);
            }
            return new UnescapeResult('x', true, false);
          }
        }
        default: {
          return new UnescapeResult(stringAfterEscMarker.charAt(0), false, true);
        }
      }
    }
  }

  public static boolean isCharAllowedForUnquotedAtom(final char chr) {
    return Character.isLetterOrDigit(chr) || chr == '_';
  }

  public static boolean isCharNotAppropriateForHex(final char chr) {
    return (chr < '0' || chr > '9') && (chr < 'a' || chr > 'f') && (chr < 'A' || chr > 'F');
  }

  public static boolean isAllowedEscapeChar(final char chr) {
    switch (chr) {
      case 7:
      case 8:
      case '\f':
      case '\n':
      case '\r':
      case '`':
      case '"':
      case 27:
      case '\t':
      case '\'':
      case 11:
        return true;
      default:
        return false;
    }
  }

  public static String escapeString(final String str, final PrologTerm.QuotingType quotingType) {
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
          if (quotingType == PrologTerm.QuotingType.BACK_QUOTED) {
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
          if (quotingType == PrologTerm.QuotingType.DOUBLE_QUOTED) {
            result.append("\\\"");
          } else {
            result.append('\"');
          }
          break;
        case '\'':
          if (quotingType == PrologTerm.QuotingType.SINGLE_QUOTED) {
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
            final String hex = Integer.toHexString(chr).toUpperCase(Locale.ENGLISH);
            for (int c = 0; c < 4 - hex.length(); c++) {
              result.append('0');
            }
            result.append(hex);
          } else {
            result.append(chr);
          }
          break;
      }
    }

    return result.toString();
  }

  public static final class UnescapeResult {
    private final boolean needsMore;
    private final boolean error;
    private final char decoded;

    UnescapeResult(final char decoded, final boolean needsMore, final boolean error) {
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
