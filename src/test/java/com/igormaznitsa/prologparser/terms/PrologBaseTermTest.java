package com.igormaznitsa.prologparser.terms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class PrologBaseTermTest {

  @Test
  public void testAbstractPrologTermString() {
    assertThrows(NullPointerException.class, () -> new StubAbstractPrologTermTest(null));

    final PrologTerm test = new StubAbstractPrologTermTest("test");
    assertEquals(-1, test.getPos());
    assertEquals(-1, test.getLine());
  }

  @Test
  public void testAbstractPrologTermStringIntInt() {
    assertThrows(NullPointerException.class, () -> new StubAbstractPrologTermTest(null, 2, 1));
    final PrologTerm test = new StubAbstractPrologTermTest("test", 2, 1);
    assertEquals(1, test.getPos());
    assertEquals(2, test.getLine());
  }

  @Test
  public void testSetStrPosition() {
    final PrologTerm test = new StubAbstractPrologTermTest("test");
    test.setPos(101);
    assertEquals(101, test.getPos());
    test.setPos(0);
    assertEquals(-1, test.getPos());
    test.setPos(-10);
    assertEquals(-1, test.getPos());
  }

  @Test
  public void testGetStrPosition() {
    final PrologTerm test = new StubAbstractPrologTermTest("test", 202, 101);
    assertEquals(101, test.getPos());
  }

  @Test
  public void testGetLineNumber() {
    final PrologTerm test = new StubAbstractPrologTermTest("test", 202, 101);
    assertEquals(202, test.getLine());
  }

  @Test
  public void testSetLineNumber() {
    final PrologTerm test = new StubAbstractPrologTermTest("test");
    test.setLine(101);
    assertEquals(101, test.getLine());
    test.setLine(0);
    assertEquals(-1, test.getLine());
    test.setPos(-10);
    assertEquals(-1, test.getLine());
  }

  @Test
  public void testGetText() {
    assertEquals("Test text", new StubAbstractPrologTermTest("Test text").getText());
  }

  @Test
  public void testGetPriority() {
    assertEquals(0, new StubAbstractPrologTermTest("test").getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals("test \n hello", new StubAbstractPrologTermTest("test \n hello").toString());
  }

  @Test
  public void testSetPayload() {
    final StubAbstractPrologTermTest stub = new StubAbstractPrologTermTest("test");
    assertNull(stub.getPayload());
    stub.setPayload("test_linked");
    assertEquals("test_linked", stub.getPayload());
    stub.setPayload(null);
    assertNull(stub.getPayload());
  }

  @Test
  public void testGetPayload() {
    final StubAbstractPrologTermTest stub = new StubAbstractPrologTermTest("test");
    assertNull(stub.getPayload());
    stub.setPayload("testObject");
    assertEquals("testObject", stub.getPayload());
  }

  private final static class StubAbstractPrologTermTest extends PrologTerm {
    private static final long serialVersionUID = 2578516387208704688L;

    public StubAbstractPrologTermTest(final String text) {
      super(text);
    }

    public StubAbstractPrologTermTest(final String text, final int line, final int pos) {
      super(text, line, pos);
    }

    @Override
    public TermType getType() {
      throw new UnsupportedOperationException("It's a stub");
    }
  }
}