package com.igormaznitsa.prologparser.terms;

import org.junit.jupiter.api.Test;

import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.*;


public class PrologIntNumberTest {

  @Test
  public void testToString() {
    final PrologInt test = new PrologInt("8192739872198741213");
    assertNotNull(test.getTermText());
    assertEquals(test.getTermText(), test.toString());
  }

  @Test
  public void testNeg() {
    assertEquals(-66324377324L, ((PrologInt) new PrologInt("66324377324").neg()).getNumber().longValue());
    assertEquals(21334324324L, ((PrologInt) new PrologInt("-21334324324").neg()).getNumber().longValue());
  }

  @Test
  public void testPrologIntegerNumberLong() {
    assertEquals(9923L, new PrologInt(9923L).getNumber().longValue());
    assertEquals(-12343L, new PrologInt(-12343L).getNumber().longValue());
  }

  @Test
  public void testPrologIntegerNumberString() {
    assertThrows(NullPointerException.class, () -> new PrologInt((String) null));

    assertThrows(IllegalArgumentException.class, () -> new PrologInt("wrong value"));
    assertThrows(IllegalArgumentException.class, () -> new PrologInt(""));

    assertEquals(78621837612L, new PrologInt("78621837612").getNumber().longValue());
    assertEquals(-121231234214L, new PrologInt("-121231234214").getNumber().longValue());
    assertEquals(0L, new PrologInt("-0").getNumber().longValue());
    assertEquals(0L, new PrologInt("-0000000").getNumber().longValue());
    assertEquals(0L, new PrologInt("00000").getNumber().longValue());
  }

  @Test
  public void testPrologIntegerNumberBigInteger() {
    assertThrows(NullPointerException.class, () -> new PrologInt((BigInteger) null));

    final BigInteger testValue = new BigInteger("829374982374812093209380194832984");

    assertEquals(testValue, new PrologInt(testValue).getIntValue());
  }

  @Test
  public void testPrologIntegerNumberStringIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologInt((String) null, 0, 0));

    final PrologTerm term = new PrologInt("123", 2, 1);
    assertEquals(1, term.getPos());
    assertEquals(2, term.getLine());
  }

  @Test
  public void testPrologIntegerNumberLongIntInt() {
    final PrologTerm term = new PrologInt(123L, 2, 1);
    assertEquals(1, term.getPos());
    assertEquals(2, term.getLine());
  }

  @Test
  public void testPrologIntegerNumberBigIntegerIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologInt((BigInteger) null, 0, 0));

    final PrologTerm term = new PrologInt(BigInteger.ONE, 2, 1);
    assertEquals(1, term.getPos());
    assertEquals(2, term.getLine());
  }

  @Test
  public void testGetValue() {
    assertEquals(9998837672L, new PrologInt(9998837672L).getNumber().longValue());
    assertEquals(-88878233243L, new PrologInt("-88878233243").getNumber().longValue());
  }

  @Test
  public void testGetType() {
    assertEquals(TermType.ATOM, new PrologInt(new BigInteger("111")).getTermType());
    assertEquals(TermType.ATOM, new PrologInt("24324324").getTermType());
  }

  @Test
  public void testGetText() {
    assertEquals("9879823432", new PrologInt(9879823432L).getTermText());
    assertEquals("-2342343243", new PrologInt(-2342343243L).getTermText());
    assertEquals("-23", new PrologInt("-0000023").getTermText());
    assertEquals("123", new PrologInt("0000000000000000000000000123").getTermText());
  }

  @Test
  public void testGetPrecedence() {
    assertEquals(0, new PrologInt("97239847324").getPrecedence());
    assertEquals(0, new PrologInt(123L).getPrecedence());
  }
}
