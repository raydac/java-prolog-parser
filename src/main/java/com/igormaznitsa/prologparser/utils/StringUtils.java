package com.igormaznitsa.prologparser.utils;

@SuppressWarnings("serial")
public final class StringUtils {

  private StringUtils() {
  }

  public static UnescapeResult tryUnescapeCharacter(final StrBuffer stringAfterEscMarker) {
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
              if (stringAfterEscMarker.hasSeveralChars() && !isCharAppropriateForHex(stringAfterEscMarker.getLastChar())) {
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
            if (stringAfterEscMarker.hasSeveralChars() && !isCharAppropriateForHex(stringAfterEscMarker.getLastChar())) {
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

  public static boolean isCharAppropriateForHex(final char chr) {
    return (chr >= '0' && chr <= '9') || (chr >= 'a' && chr <= 'f') || (chr >= 'A' && chr <= 'F');
  }

  public static String escapeString(final String str) {
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
          result.append("\\`");
          break;
        case 27:
          result.append("\\e");
          break;
        case '\t':
          result.append("\\t");
          break;
        case '\'':
          result.append("\\'");
          break;
        case 11:
          result.append("\\v");
          break;
        default:
          result.append(chr);
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
