package com.igormaznitsa.prologparser.utils;

import static com.igormaznitsa.prologparser.terms.Quotation.SINGLE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;


public class StringUtilsTest {

  private void assertUnescaped(int expectedCodePoint, String testString) {
    final StringUtils.UnescapeResult result =
        StringUtils.tryUnescapeCharacter(new StringBuilderEx(testString));
    assertFalse(result.isError());
    assertFalse(result.doesNeedMore());
    assertEquals(expectedCodePoint, result.getDecoded());
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
  }

  @Test
  public void testUnescapeCodePointAboveBasicPlane() {
    assertUnescaped(0x20000, "x20000\\");
    assertUnescaped(0x10FFFF, "x10FFFF\\");
    assertUnescaped(0x20000, "400000\\");
    assertUnescaped(0x1F600, "U0001F600");
    assertUnescaped(0x4E2D, "U00004e2d");

    assertNotFullData("U");
    assertNotFullData("Ubbbb");
    assertNotFullData("U0001F60");

    assertErrorUnescaped("x110000\\");
    assertErrorUnescaped("4200000\\");
    assertErrorUnescaped("U00110000");
    assertErrorUnescaped("UFFFFFFFF");
    assertErrorUnescaped("U0001F600F");
    assertErrorUnescaped("U0001F6z");
  }

  @Test
  public void testAlphanumericIncludesCombiningMarks() {
    assertTrue(StringUtils.isAlphanumeric('a'));
    assertTrue(StringUtils.isAlphanumeric('7'));
    assertTrue(StringUtils.isAlphanumeric('中'));
    assertTrue(StringUtils.isAlphanumeric(0x20000));
    assertTrue(StringUtils.isAlphanumeric(0x0301));
    assertTrue(StringUtils.isAlphanumeric(0x093F));
    assertTrue(StringUtils.isAlphanumeric(0x20DD));

    assertFalse(StringUtils.isAlphanumeric('_'));
    assertFalse(StringUtils.isAlphanumeric('+'));
    assertFalse(StringUtils.isAlphanumeric(0x1F600));
    assertFalse(StringUtils.isAlphanumeric(0x3000));

    assertTrue(StringUtils.isCharAllowedForUnquotedAtom('_'));
    assertFalse(StringUtils.isCharAllowedForUnquotedAtom('-'));
  }

  @Test
  public void testEscapeString() {
    final String test = "Hello\r'World'\nAnd Skolkovo`\f too\t\u0007\u001b\u000b";

    assertEquals(StringUtils.escapeString(test, SINGLE),
        "Hello\\r\\'World\\'\\nAnd Skolkovo`\\f too\\t\\a\\e\\v");
  }
}
