package com.igormaznitsa.prologparser.terms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class PrologVarTest {

  @Test
  public void testGetType() {
    final PrologVar var = new PrologVar("X");
    assertEquals(TermType.VAR, var.getTermType());

    final PrologVar var2 = new PrologVar();
    assertEquals(TermType.VAR, var2.getTermType());
  }

  @Test
  public void testPrologVariable() {
    final PrologVar var = new PrologVar();
    assertTrue(var.isAnonymous());
  }

  @Test
  public void testPrologVariableIntInt() {
    final PrologVar var = new PrologVar(2, 1);
    assertTrue(var.isAnonymous());
    assertEquals(1, var.getPos());
    assertEquals(2, var.getLine());
  }

  @Test
  public void testPrologVariableString() {
    assertThrows(NullPointerException.class, () -> new PrologVar(null));
    assertThrows(IllegalArgumentException.class, () -> new PrologVar(""));
    assertThrows(IllegalArgumentException.class, () -> new PrologVar("привет"));
    assertThrows(IllegalArgumentException.class, () -> new PrologVar("abc"));

    PrologVar var = new PrologVar("X");
    assertFalse(var.isAnonymous());
    assertEquals("X", var.getTermText());

    var = new PrologVar("_");
    assertTrue(var.isAnonymous());
    assertEquals("_", var.getTermText());

    var = new PrologVar("_hello_world");
    assertFalse(var.isAnonymous());
    assertEquals("_hello_world", var.getTermText());

    var = new PrologVar("Привет");
    assertFalse(var.isAnonymous());
    assertEquals("Привет", var.getTermText());
  }

  @Test
  public void testPrologVariableStringIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologVar(null, 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologVar("", 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologVar("привет", 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologVar("abc", 2, 1));

    PrologVar var = new PrologVar("X", 2, 1);

    assertEquals(1, var.getPos());
    assertEquals(2, var.getLine());
  }

  @Test
  public void testIsAnonymous() {
    PrologVar var = new PrologVar();
    assertTrue(var.isAnonymous());
    var = new PrologVar("_");
    assertTrue(var.isAnonymous());
    var = new PrologVar("Ddsd");
    assertFalse(var.isAnonymous());
  }

  @Test
  public void testGetText() {
    assertEquals("_", new PrologVar().getTermText());
    assertEquals("_", new PrologVar("_").getTermText());
    assertEquals("X", new PrologVar("X").getTermText());
    assertEquals("Variable", new PrologVar("Variable").getTermText());
  }

  @Test
  public void testGetPrecedence() {
    final PrologVar var = new PrologVar("Hello");
    assertEquals(0, var.getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals("_", new PrologVar().toString());
    assertEquals("_", new PrologVar("_").toString());
    assertEquals("__________test",
        new PrologVar("__________test").toString());
    assertEquals("Abc", new PrologVar("Abc").toString());
    assertEquals("Привет", new PrologVar("Привет").toString());
  }
}
