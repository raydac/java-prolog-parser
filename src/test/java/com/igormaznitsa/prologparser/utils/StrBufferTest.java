package com.igormaznitsa.prologparser.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class StrBufferTest {

  @Test
  public void testHasSingleChar() {
    final StringBuilderEx bldr = new StringBuilderEx(10);
    bldr.append('a');
    assertTrue(bldr.isSingleChar('a'));
    assertFalse(bldr.isSingleChar('b'));
  }

  @Test
  public void testIsLastChar() {
    final StringBuilderEx bldr = new StringBuilderEx(10);
    bldr.append("hello");
    assertTrue(bldr.isLastChar('o'));
    assertFalse(bldr.isLastChar('a'));
  }

  @Test
  public void testToStringExcludeLastChar() {
    final StringBuilderEx bldr = new StringBuilderEx(10);
    bldr.append("hello");
    assertEquals("hell", bldr.toStringExcludeLastChar());
  }

  @Test
  public void testAppend() {
    final StringBuilderEx bldr = new StringBuilderEx(3);
    bldr.append("hello");
    bldr.append("world");
    assertEquals("helloworld", bldr.toString());
  }

  @Test
  public void testClear() {
    final StringBuilderEx bldr = new StringBuilderEx(3);
    bldr.append("hello");
    bldr.clear();
    assertEquals("", bldr.toString());
  }

}
