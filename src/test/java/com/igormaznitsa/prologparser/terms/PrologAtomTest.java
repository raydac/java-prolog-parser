package com.igormaznitsa.prologparser.terms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class PrologAtomTest {

  @Test
  public void testGetPriority() {
    assertEquals(new PrologAtom("Hello").getPrecedence(), 0);
  }

  @Test
  public void testToString() {
    assertEquals("\'Hello World\'", new PrologAtom("Hello World").toString());
    assertEquals("\'Hello\\nWorld\'", new PrologAtom("Hello\nWorld").toString());
    assertEquals("\'Hello\\\\nWorld\'", new PrologAtom("Hello\\\nWorld").toString());
    assertEquals("\'Hello\\tWorld\'", new PrologAtom("Hello\tWorld").toString());
    assertEquals("\'!\'", new PrologAtom("!").toString());
  }

  @Test
  public void testGetType() {
    assertEquals(TermType.ATOM, new PrologAtom("Hello Prolog").getTermType());
  }

  @Test
  public void testPrologAtom_String_NPE() {
    assertThrows(NullPointerException.class, () -> new PrologAtom((String) null));
  }

  @Test
  public void testPrologAtom_Term_NPE() {
    assertThrows(NullPointerException.class, () -> new PrologAtom((PrologTerm) null));
  }

  @Test
  public void testPrologAtom_String() {
    final PrologAtom atom = new PrologAtom("test");
    assertEquals("test", atom.getTermText());
  }

  @Test
  public void testPrologAtom_Term() {
    final PrologAtom etalon = new PrologAtom("etal", 222, 111);

    final PrologAtom atom = new PrologAtom(etalon);
    assertEquals("etal", atom.getTermText());
    assertEquals(111, atom.getPos());
    assertEquals(222, atom.getLine());
  }

  @Test
  public void testPrologAtomStringIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologAtom(null, 0, 0));

    final PrologTerm term = new PrologAtom("test", 2, 1);
    assertEquals(1, term.getPos());
    assertEquals(2, term.getLine());
  }
}
