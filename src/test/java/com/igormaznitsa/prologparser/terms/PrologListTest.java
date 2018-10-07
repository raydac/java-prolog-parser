package com.igormaznitsa.prologparser.terms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class PrologListTest {

  @Test
  public void testToString() {
    assertEquals(new PrologList(new PrologAtom("test")).toString(),
        "[\'test\']");

    String[] atoms = new String[] {"test1", "test2", "test3", "test4"};
    StringBuilder builder = new StringBuilder();
    PrologTerm[] terms = new PrologTerm[atoms.length];
    int index = 0;
    for (final String atom : atoms) {
      terms[index++] = new PrologAtom(atom);
      if (builder.length() > 0) {
        builder.append(", ");
      }
      builder.append('\'' + atom + '\'');
    }

    assertEquals('[' + builder.toString() + ']',
        new PrologList(terms).toString());

    assertEquals(new PrologList(new PrologAtom("hello"),
        new PrologVariable("X")).toString(), "[\'hello\'|X]");
    assertEquals(new PrologList(new PrologAtom("hello"), new PrologList(
        new PrologVariable("X"))).toString(), "[\'hello\', X]");
    assertEquals(new PrologList().toString(), "[]");

    PrologList list = new PrologList();
    list.setTail(new PrologAtom("test"));
    assertEquals("[\'\'|\'test\']", list.toString());

    list = new PrologList();
    list.setHead(new PrologAtom("test"));
    assertEquals("[\'test\']", list.toString());
  }

  @Test
  public void testGetType() {
    PrologList list = new PrologList();
    assertEquals(TermType.LIST, list.getTermType());

    list = new PrologList(new PrologAtom("Test"));
    assertEquals(TermType.LIST, list.getTermType());

    list = new PrologList(new PrologAtom("Test"), new PrologAtom("Test2"));
    assertEquals(TermType.LIST, list.getTermType());
  }

  @Test
  public void testPrologList() {
    final PrologList list = new PrologList();
    assertTrue(list.isNullList());
    assertNull(list.getElementAt(0));
    assertNull(list.getElementAt(1));
    assertEquals(2, list.getArity());
  }

  @Test
  public void testPrologListIntInt() {
    final PrologList list = new PrologList(2, 1);
    assertEquals(1, list.getPos());
    assertEquals(2, list.getLine());
  }

  @Test
  public void testPrologListAbstractPrologTermArray() {
    assertThrows(NullPointerException.class, () -> new PrologList((PrologTerm[]) null));

    String[] atoms = new String[] {"test1", "test2", "test3", "test4"};
    PrologTerm[] terms = new PrologTerm[atoms.length];
    int index = 0;
    for (final String atom : atoms) {
      terms[index++] = new PrologAtom(atom);
    }

    PrologList list = new PrologList(terms);
    for (final String text : atoms) {
      assertEquals(text, list.getHead().getTermText());
      list = (PrologList) list.getTail();
    }
    assertTrue(list.isNullList());

    atoms = new String[] {"test1", null, "test3", "test4"};
    terms = new PrologTerm[atoms.length];
    index = 0;
    for (final String atom : atoms) {
      terms[index++] = atom == null ? null : new PrologAtom(atom);
    }

    final PrologTerm[] finalTerms = terms;
    assertThrows(NullPointerException.class, () -> new PrologList(finalTerms));
  }

  @Test
  public void testPrologListAbstractPrologTermArrayIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologList((PrologTerm[]) null, 2, 1));

    final PrologList list = new PrologList(new PrologTerm[] {new PrologAtom("test")}, 2, 1);
    assertEquals(1, list.getPos());
    assertEquals(2, list.getLine());

  }

  @Test
  public void testPrologListAbstractPrologTerm() {
    assertThrows(NullPointerException.class, () -> new PrologList((PrologTerm) null));

    final PrologAtom atom = new PrologAtom("test");
    final PrologList list = new PrologList(atom);

    assertFalse(list.isNullList());
    assertEquals(atom, list.getHead());
    assertNotNull(list.getTail());
    assertTrue(((PrologList) list.getTail()).isNullList());
  }

  @Test
  public void testPrologListAbstractPrologTermIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologList((PrologTerm) null, 2, 1));

    final PrologAtom atom = new PrologAtom("test");
    final PrologList list = new PrologList(atom, 2, 1);

    assertEquals(1, list.getPos());
    assertEquals(2, list.getLine());
  }

  @Test
  public void testPrologListAbstractPrologTermAbstractPrologTerm() {
    assertThrows(NullPointerException.class, () -> new PrologList(null, null));
    assertThrows(NullPointerException.class, () -> new PrologList(null, new PrologAtom("test")));
    assertThrows(NullPointerException.class, () -> new PrologList(new PrologAtom("test"), null));

    final PrologAtom atom1 = new PrologAtom("test");
    final PrologAtom atom2 = new PrologAtom("test2");
    final PrologList list = new PrologList(atom1, atom2);
    assertSame(atom1, list.getHead());
    assertSame(atom2, list.getTail());
  }

  @Test
  public void testPrologListAbstractPrologTermAbstractPrologTermIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologList(null, null, 2, 1));
    assertThrows(NullPointerException.class, () -> new PrologList(null, new PrologAtom("test"), 2, 1));
    assertThrows(NullPointerException.class, () -> new PrologList(new PrologAtom("test"), null, 2, 1));

    final PrologAtom atom1 = new PrologAtom("test");
    final PrologAtom atom2 = new PrologAtom("test2");
    final PrologList list = new PrologList(atom1, atom2, 2, 1);
    assertEquals(1, list.getPos());
    assertEquals(2, list.getLine());
  }

  @Test
  public void testIsNullList() {
    final PrologList list = new PrologList();
    assertTrue(list.isNullList());
    assertNull(list.getHead());
    assertNull(list.getTail());
  }

  @Test
  public void testGetHead() {
    final PrologAtom atom = new PrologAtom("test");
    final PrologList list = new PrologList(atom);
    assertFalse(list.isNullList());
    assertSame(atom, list.getHead());
  }

  @Test
  public void testGetTail() {
    final PrologAtom atom = new PrologAtom("test");
    final PrologAtom tail = new PrologAtom("tail");
    final PrologList list1 = new PrologList(atom);
    final PrologList list2 = new PrologList(atom, tail);

    assertFalse(list1.isNullList());
    assertNotNull(list1.getTail());
    assertTrue(((PrologList) list1.getTail()).isNullList());

    assertFalse(list2.isNullList());
    assertNotNull(list2.getTail());
    assertSame(tail, list2.getTail());
  }

  @Test
  public void testSetHead() {
    final PrologList list = new PrologList();
    final PrologAtom atom = new PrologAtom("test");

    assertThrows(NullPointerException.class, () -> list.setHead(null));

    assertTrue(list.isNullList());
    list.setHead(atom);
    assertFalse(list.isNullList());
    assertNotNull(list.getHead());
    assertSame(atom, list.getHead());
    assertNotNull(list.getTail());
    assertTrue(((PrologList) list.getTail()).isNullList());
  }

  @Test
  public void testSetTail() {
    final PrologList list = new PrologList();
    final PrologAtom atom = new PrologAtom("test");

    assertThrows(NullPointerException.class, () -> list.setTail(null));

    assertTrue(list.isNullList());
    list.setTail(atom);
    assertFalse(list.isNullList());
    assertNotNull(list.getTail());
    assertSame(atom, list.getTail());
    assertSame(PrologList.EMPTY_ATOM, list.getHead());
  }

  @Test
  public void testSetTermAsNewListTail() {
    final PrologList list = new PrologList();
    final PrologAtom atom = new PrologAtom("test");

    assertTrue(list.isNullList());
    assertThrows(NullPointerException.class, () -> PrologList.setTermAsNewListTail(list, null));

    PrologList newList = PrologList.setTermAsNewListTail(list, atom);
    assertFalse(list.isNullList());
    assertSame(list, newList);
    assertSame(atom, list.getHead());
    assertNotNull(list.getTail());
    assertTrue(((PrologList) list.getTail()).isNullList());

    newList = PrologList.setTermAsNewListTail(list, atom);
    assertNotSame(list, newList);
    assertSame(newList, list.getTail());
    assertSame(atom, newList.getHead());
    assertTrue(((PrologList) newList.getTail()).isNullList());
  }

  @Test
  public void testAddAsNewListToEndOfListChain() {
    final PrologList list = new PrologList(new PrologAtom("test1"));
    PrologList list2 = PrologList.setTermAsNewListTail(list,
        new PrologAtom("test2"));
    PrologList list3 = PrologList.setTermAsNewListTail(list2,
        new PrologAtom("test3"));

    assertSame(list2, list.getTail());
    assertSame(list3, list2.getTail());
    assertTrue(((PrologList) list3.getTail()).isNullList());

    final PrologAtom atom = new PrologAtom("hello");

    assertThrows(NullPointerException.class, () -> list.addAsNewListToEndOfListChain(null));

    list.addAsNewListToEndOfListChain(atom);
    assertFalse(((PrologList) list3.getTail()).isNullList());
    PrologList newlist = (PrologList) list3.getTail();
    assertSame(atom, newlist.getHead());
    assertTrue(((PrologList) newlist.getTail()).isNullList());

    final PrologList listn = new PrologList(new PrologAtom("first"),
        new PrologAtom("second"));
    final PrologAtom third = new PrologAtom("third");

    newlist = listn.addAsNewListToEndOfListChain(third);
    assertSame(newlist, listn.getTail());
    assertSame(third, newlist.getHead());
    assertTrue(((PrologList) newlist.getTail()).isNullList());

  }

  @Test
  public void testReplaceLastElement() {
    final PrologList list = new PrologList(new PrologAtom("test1"));
    PrologList list2 = PrologList.setTermAsNewListTail(list,
        new PrologAtom("test2"));
    PrologList list3 = PrologList.setTermAsNewListTail(list2,
        new PrologAtom("test3"));

    assertSame(list2, list.getTail());
    assertSame(list3, list2.getTail());
    assertTrue(((PrologList) list3.getTail()).isNullList());

    final PrologAtom atom = new PrologAtom("hello");

    list.replaceTail(atom);
    assertSame(atom, list3.getTail());

    list3.setTail(new PrologVariable());
    final PrologAtom third = new PrologAtom("third");
    list.replaceTail(third);
    assertSame(third, list3.getTail());
  }
}
