package com.igormaznitsa.prologparser.utils;

import static com.igormaznitsa.prologparser.terms.Quotation.SINGLE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;


public class StringUtilsTest {

  private void assertUnescaped(char expectedChar, String testString) {
    final StringUtils.UnescapeResult result =
        StringUtils.tryUnescapeCharacter(new StringBuilderEx(testString));
    assertFalse(result.isError());
    assertFalse(result.doesNeedMore());
    assertEquals(expectedChar, result.getDecoded());
  }

  private void assertErrorUnescaped(String testString) {
    final StringUtils.UnescapeResult result =
        StringUtils.tryUnescapeCharacter(new StringBuilderEx(testString));
    assertTrue(result.isError());
  }

  private void assertNotFullData(String testString) {
    final StringUtils.UnescapeResult result =
        StringUtils.tryUnescapeCharacter(new StringBuilderEx(testString));
    assertFalse(result.isError());
    assertTrue(result.doesNeedMore());
  }

  @Test
  public void testUnescapeCharacter() {
    assertUnescaped((char) 7, "a");
    assertUnescaped((char) 8, "b");
    assertUnescaped((char) 27, "e");
    assertUnescaped((char) 11, "v");
    assertUnescaped('\r', "r");
    assertUnescaped('\n', "n");
    assertUnescaped('\\', "\\");
    assertUnescaped('\'', "'");
    assertUnescaped('\f', "f");
    assertUnescaped('\t', "t");
    assertUnescaped('`', "`");
    assertUnescaped(' ', "s");
    assertUnescaped((char) 32, "x20\\");
    assertUnescaped((char) 0xFF00, "uFF00");
    assertUnescaped((char) 0xFF0A, "uFF0a");
    assertUnescaped((char) 0xFF0A, "ufF0A");
    assertUnescaped((char) 0xBBBB, "ubbbb");

    assertNotFullData("x12234");
    assertNotFullData("xa");
    assertNotFullData("x2023");
    assertNotFullData("uFF0");

    assertErrorUnescaped("uFFm");
    assertErrorUnescaped("xm");

    assertErrorUnescaped("z");
    assertErrorUnescaped("Xff");
    assertErrorUnescaped("xffx\\");
    assertErrorUnescaped("ufF0AC2");
    assertErrorUnescaped("uFF0z");
    assertErrorUnescaped("uFF0z");
    assertErrorUnescaped("Ubbbb");
  }

  @Test
  public void testEscapeString() {
    final String test = "Hello\r'World'\nAnd Skolkovo`\f too\t\u0007\u001b\u000b";

    assertEquals(StringUtils.escapeString(test, SINGLE),
        "Hello\\r\\'World\\'\\nAnd Skolkovo`\\f too\\t\\a\\e\\v");
  }
}
