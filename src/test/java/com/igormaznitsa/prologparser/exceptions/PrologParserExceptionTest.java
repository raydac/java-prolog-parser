package com.igormaznitsa.prologparser.exceptions;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class PrologParserExceptionTest {

  @Test
  public void testPrologParserException() {
    try {
      throw new PrologParserException("Hello World", 110, 32);
    } catch (PrologParserException ex) {
      assertEquals("Hello World", ex.getMessage());
    }
  }

  @Test
  public void testGetLineNumber() {
    try {
      throw new PrologParserException("Hello world", 110, 32);
    } catch (PrologParserException ex) {
      assertEquals(110, ex.getLine());
    }
  }

  @Test
  public void testGetStringPosition() {
    try {
      throw new PrologParserException("Hello world", 110, 32);
    } catch (PrologParserException ex) {
      assertEquals(32, ex.getPos());
    }
  }

  @Test
  public void testContainsRightPositionData() {
    try {
      throw new PrologParserException("Hello world", -1, 0);
    } catch (PrologParserException ex) {
      assertFalse(ex.hasValidPosition());
    }

    try {
      throw new PrologParserException("Hello world", -1, -1);
    } catch (PrologParserException ex) {
      assertFalse(ex.hasValidPosition());
    }

    try {
      throw new PrologParserException("Hello world", 0, 0);
    } catch (PrologParserException ex) {
      assertFalse(ex.hasValidPosition());
    }

    try {
      throw new PrologParserException("Hello world", 12, -1);
    } catch (PrologParserException ex) {
      assertFalse(ex.hasValidPosition());
    }

    try {
      throw new PrologParserException("Hello world", 1, 10);
    } catch (PrologParserException ex) {
      assertTrue(ex.hasValidPosition());
    }
  }

  @Test
  public void testToString() {
    assertEquals("Hello World[1:10]", new PrologParserException("Hello World", 1, 10).toString());
  }
}
