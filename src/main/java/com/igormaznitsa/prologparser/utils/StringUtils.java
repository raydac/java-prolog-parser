package com.igormaznitsa.prologparser.utils;

import java.util.concurrent.atomic.AtomicReference;

@SuppressWarnings("serial")
public final class StringUtils {

  private StringUtils() {
  }

  public static boolean unescapeCharacter(final String afterString, final AtomicReference<Character> result) {
    if (afterString == null) {
      result.set(null);
      return false;
    }

    final int len = afterString.length();

    if (len == 1) {
      boolean flag = true;
      switch (afterString.charAt(0)) {
        case 'a':
          result.set((char) 7);
          break;
        case 'b':
          result.set('\b');
          break;
        case 'n':
          result.set('\n');
          break;
        case 'f':
          result.set('\f');
          break;
        case 'r':
          result.set('\r');
          break;
        case 'e':
          result.set((char) 27);
          break;
        case 't':
          result.set('\t');
          break;
        case 's':
          result.set((char) 32);
          break;
        case 'v':
          result.set((char) 11);
          break;
        case '\\':
          result.set('\\');
          break;
        case '\'':
          result.set('\'');
          break;
        case '`':
          result.set('`');
          break;
        case 'u':
        case 'x':
          result.set(null);
          break;
        default:
          result.set(null);
          flag = false;
          break;
      }
      return flag;
    } else {
      switch (afterString.charAt(0)) {
        case 'u': {
          int num;
          try {
            num = Integer.parseInt(afterString.substring(1), 16);
          } catch (NumberFormatException ex) {
            result.set(null);
            return false;
          }

          if (len == 5) {
            result.set((char) num);
            return true;
          } else {
            if (len > 5) {
              result.set(null);
              return false;
            }
            result.set(null);
            return true;
          }
        }
        case 'x': {
          int num;
          try {
            num = Integer.parseInt(afterString.substring(1), 16);
          } catch (NumberFormatException ex) {
            result.set(null);
            return false;
          }

          if (len == 3) {
            result.set((char) num);
            return true;
          } else {
            if (len > 3) {
              result.set(null);
              return false;
            }
            result.set(null);
            return true;
          }
        }
        default: {
          result.set(null);
          return false;
        }
      }
    }
  }

  public static String escapeString(final String str) {
    final StrBuffer result = new StrBuffer(str.length() << 1);

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
}
