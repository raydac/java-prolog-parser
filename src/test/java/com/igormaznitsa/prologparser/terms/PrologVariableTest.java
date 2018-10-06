package com.igormaznitsa.prologparser.terms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class PrologVariableTest {

  @Test
  public void testGetType() {
    final PrologVariable var = new PrologVariable("X");
    assertEquals(TermType.VAR, var.getType());

    final PrologVariable var2 = new PrologVariable();
    assertEquals(TermType.VAR, var2.getType());
  }

  @Test
  public void testPrologVariable() {
    final PrologVariable var = new PrologVariable();
    assertTrue(var.isAnonymous());
  }

  @Test
  public void testPrologVariableIntInt() {
    final PrologVariable var = new PrologVariable(2, 1);
    assertTrue(var.isAnonymous());
    assertEquals(1, var.getPos());
    assertEquals(2, var.getLine());
  }

  @Test
  public void testPrologVariableString() {
    assertThrows(NullPointerException.class, () -> new PrologVariable(null));
    assertThrows(IllegalArgumentException.class, () -> new PrologVariable(""));
    assertThrows(IllegalArgumentException.class, () -> new PrologVariable("привет"));
    assertThrows(IllegalArgumentException.class, () -> new PrologVariable("abc"));

    PrologVariable var = new PrologVariable("X");
    assertFalse(var.isAnonymous());
    assertEquals("X", var.getText());

    var = new PrologVariable("_");
    assertTrue(var.isAnonymous());
    assertEquals("_", var.getText());

    var = new PrologVariable("_hello_world");
    assertFalse(var.isAnonymous());
    assertEquals("_hello_world", var.getText());

    var = new PrologVariable("Привет");
    assertFalse(var.isAnonymous());
    assertEquals("Привет", var.getText());
  }

  @Test
  public void testPrologVariableStringIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologVariable(null, 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologVariable("", 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologVariable("привет", 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologVariable("abc", 2, 1));

    PrologVariable var = new PrologVariable("X", 2, 1);

    assertEquals(1, var.getPos());
    assertEquals(2, var.getLine());
  }

  @Test
  public void testIsAnonymous() {
    PrologVariable var = new PrologVariable();
    assertTrue(var.isAnonymous());
    var = new PrologVariable("_");
    assertTrue(var.isAnonymous());
    var = new PrologVariable("Ddsd");
    assertFalse(var.isAnonymous());
  }

  @Test
  public void testGetText() {
    assertEquals("_", new PrologVariable().getText());
    assertEquals("_", new PrologVariable("_").getText());
    assertEquals("X", new PrologVariable("X").getText());
    assertEquals("Variable", new PrologVariable("Variable").getText());
  }

  @Test
  public void testGetPriority() {
    final PrologVariable var = new PrologVariable("Hello");
    assertEquals(0, var.getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals("_", new PrologVariable().toString());
    assertEquals("_", new PrologVariable("_").toString());
    assertEquals("__________test",
        new PrologVariable("__________test").toString());
    assertEquals("Abc", new PrologVariable("Abc").toString());
    assertEquals("Привет", new PrologVariable("Привет").toString());
  }
}
