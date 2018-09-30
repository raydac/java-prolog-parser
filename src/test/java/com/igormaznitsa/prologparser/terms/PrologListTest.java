/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class PrologListTest extends AbstractPrologParserTest {

  @Test
  public void testToString() {
    assertEquals(new PrologList(new PrologAtom("test")).toString(),
        "[\'test\']");

    String[] atoms = new String[] {"test1", "test2", "test3", "test4"};
    StringBuilder builder = new StringBuilder();
    AbstractPrologTerm[] terms = new AbstractPrologTerm[atoms.length];
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
    assertEquals(PrologTermType.LIST, list.getType());

    list = new PrologList(new PrologAtom("Test"));
    assertEquals(PrologTermType.LIST, list.getType());

    list = new PrologList(new PrologAtom("Test"), new PrologAtom("Test2"));
    assertEquals(PrologTermType.LIST, list.getType());
  }

  @Test
  public void testPrologList() {
    final PrologList list = new PrologList();
    assertTrue(list.isNullList());
    assertNull(list.getElement(0));
    assertNull(list.getElement(1));
    assertEquals(2, list.getArity());
  }

  @Test
  public void testPrologListIntInt() {
    final PrologList list = new PrologList(1, 2);
    assertEquals(1, list.getStrPosition());
    assertEquals(2, list.getLineNumber());
  }

  @Test
  public void testPrologListAbstractPrologTermArray() {
    try {
      new PrologList((AbstractPrologTerm[]) null);
      fail("The part must throw NPE for the null as the array");
    } catch (NullPointerException ex) {
    }

    String[] atoms = new String[] {"test1", "test2", "test3", "test4"};
    AbstractPrologTerm[] terms = new AbstractPrologTerm[atoms.length];
    int index = 0;
    for (final String atom : atoms) {
      terms[index++] = new PrologAtom(atom);
    }

    PrologList list = new PrologList(terms);
    index = 0;
    for (final String text : atoms) {
      assertEquals(text, list.getHead().getText());
      list = (PrologList) list.getTail();
    }
    assertTrue(list.isNullList());

    atoms = new String[] {"test1", null, "test3", "test4"};
    terms = new AbstractPrologTerm[atoms.length];
    index = 0;
    for (final String atom : atoms) {
      terms[index++] = atom == null ? null : new PrologAtom(atom);
    }
    try {
      new PrologList(terms);
      fail("The part must throw NPE for a null element at the array");
    } catch (NullPointerException ex) {
    }
  }

  @Test
  public void testPrologListAbstractPrologTermArrayIntInt() {
    try {
      new PrologList((AbstractPrologTerm[]) null, 1, 2);
      fail("The part must throw NPE for the null as the array");
    } catch (NullPointerException ex) {
    }

    final PrologList list = new PrologList(new AbstractPrologTerm[] {new PrologAtom("test")}, 1, 2);
    assertEquals(1, list.getStrPosition());
    assertEquals(2, list.getLineNumber());

  }

  @Test
  public void testPrologListAbstractPrologTerm() {
    try {
      new PrologList((AbstractPrologTerm) null);
      fail("Must throw NPE for null term");
    } catch (NullPointerException ex) {
    }

    final PrologAtom atom = new PrologAtom("test");
    final PrologList list = new PrologList(atom);

    assertFalse(list.isNullList());
    assertEquals(atom, list.getHead());
    assertNotNull(list.getTail());
    assertTrue(((PrologList) list.getTail()).isNullList());
  }

  @Test
  public void testPrologListAbstractPrologTermIntInt() {
    try {
      new PrologList((AbstractPrologTerm) null, 1, 2);
      fail("Must throw NPE for null term");
    } catch (NullPointerException ex) {
    }

    final PrologAtom atom = new PrologAtom("test");
    final PrologList list = new PrologList(atom, 1, 2);

    assertEquals(1, list.getStrPosition());
    assertEquals(2, list.getLineNumber());
  }

  @Test
  public void testPrologListAbstractPrologTermAbstractPrologTerm() {
    try {
      new PrologList(null, null);
      fail("The part must throw NPE for null elements");
    } catch (NullPointerException ex) {
    }

    try {
      new PrologList(null, new PrologAtom("test"));
      fail("The part must throw NPE for the null as the head");
    } catch (NullPointerException ex) {
    }

    try {
      new PrologList(new PrologAtom("test"), null);
      fail("The part must throw NPE for the null as the tail");
    } catch (NullPointerException ex) {
    }

    final PrologAtom atom1 = new PrologAtom("test");
    final PrologAtom atom2 = new PrologAtom("test2");
    final PrologList list = new PrologList(atom1, atom2);
    assertSame(atom1, list.getHead());
    assertSame(atom2, list.getTail());
  }

  @Test
  public void testPrologListAbstractPrologTermAbstractPrologTermIntInt() {
    try {
      new PrologList(null, null, 1, 2);
      fail("The part must throw NPE for null elements");
    } catch (NullPointerException ex) {
    }

    try {
      new PrologList(null, new PrologAtom("test"), 1, 2);
      fail("The part must throw NPE for the null as the head");
    } catch (NullPointerException ex) {
    }

    try {
      new PrologList(new PrologAtom("test"), null, 1, 2);
      fail("The part must throw NPE for the null as the tail");
    } catch (NullPointerException ex) {
    }

    final PrologAtom atom1 = new PrologAtom("test");
    final PrologAtom atom2 = new PrologAtom("test2");
    final PrologList list = new PrologList(atom1, atom2, 1, 2);
    assertEquals(1, list.getStrPosition());
    assertEquals(2, list.getLineNumber());
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

    try {
      list.setHead(null);
      fail("Must throw NPE for the null as head");
    } catch (NullPointerException ex) {
    }

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

    try {
      list.setTail(null);
      fail("Must throw NPE for the null as tail");
    } catch (NullPointerException ex) {
    }

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

    try {
      PrologList.setTermAsNewListTail(list, null);
      fail("Must throw NPE for the null as the new tail");
    } catch (NullPointerException ex) {
    }

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

    try {
      list.addAsNewListToEndOfListChain(null);
      fail("Must throw NPE for null value");
    } catch (NullPointerException ex) {
    }

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

    list.replaceLastElement(atom);
    assertSame(atom, list3.getTail());

    list3.setTail(new PrologVariable());
    final PrologAtom third = new PrologAtom("third");
    list.replaceLastElement(third);
    assertSame(third, list3.getTail());
  }
}
