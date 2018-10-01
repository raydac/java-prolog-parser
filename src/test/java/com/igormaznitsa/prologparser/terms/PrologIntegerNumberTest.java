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

import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.*;


public class PrologIntegerNumberTest extends AbstractPrologParserTest {

  @Test
  public void testToString() {
    final PrologIntegerNumber test = new PrologIntegerNumber("8192739872198741213");
    assertNotNull(test.getText());
    assertEquals(test.getText(), test.toString());
  }

  @Test
  public void testNeg() {
    assertEquals(-66324377324L, ((PrologIntegerNumber) new PrologIntegerNumber("66324377324").neg()).getValue().longValue());
    assertEquals(21334324324L, ((PrologIntegerNumber) new PrologIntegerNumber("-21334324324").neg()).getValue().longValue());
  }

  @Test
  public void testPrologIntegerNumberLong() {
    assertEquals(9923L, new PrologIntegerNumber(9923L).getValue().longValue());
    assertEquals(-12343L, new PrologIntegerNumber(-12343L).getValue().longValue());
  }

  @Test
  public void testPrologIntegerNumberString() {
    assertThrows(NullPointerException.class, () -> new PrologIntegerNumber((String) null));

    assertThrows(IllegalArgumentException.class, () -> new PrologIntegerNumber("wrong value"));
    assertThrows(IllegalArgumentException.class, () -> new PrologIntegerNumber(""));

    assertEquals(78621837612L, new PrologIntegerNumber("78621837612").getValue().longValue());
    assertEquals(-121231234214L, new PrologIntegerNumber("-121231234214").getValue().longValue());
    assertEquals(0L, new PrologIntegerNumber("-0").getValue().longValue());
    assertEquals(0L, new PrologIntegerNumber("-0000000").getValue().longValue());
    assertEquals(0L, new PrologIntegerNumber("00000").getValue().longValue());
  }

  @Test
  public void testPrologIntegerNumberBigInteger() {
    assertThrows(NullPointerException.class, () -> new PrologIntegerNumber((BigInteger) null));

    final BigInteger testValue = new BigInteger("829374982374812093209380194832984");

    assertEquals(testValue, new PrologIntegerNumber(testValue).getValue());
  }

  @Test
  public void testPrologIntegerNumberStringIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologIntegerNumber((String) null, 0, 0));

    final AbstractPrologTerm term = new PrologIntegerNumber("123", 1, 2);
    assertEquals(1, term.getStrPosition());
    assertEquals(2, term.getLineNumber());
  }

  @Test
  public void testPrologIntegerNumberLongIntInt() {
    final AbstractPrologTerm term = new PrologIntegerNumber(123L, 1, 2);
    assertEquals(1, term.getStrPosition());
    assertEquals(2, term.getLineNumber());
  }

  @Test
  public void testPrologIntegerNumberBigIntegerIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologIntegerNumber((BigInteger) null, 0, 0));

    final AbstractPrologTerm term = new PrologIntegerNumber(BigInteger.ONE, 1, 2);
    assertEquals(1, term.getStrPosition());
    assertEquals(2, term.getLineNumber());
  }

  @Test
  public void testGetValue() {
    assertEquals(9998837672L, new PrologIntegerNumber(9998837672L).getValue().longValue());
    assertEquals(-88878233243L, new PrologIntegerNumber("-88878233243").getValue().longValue());
  }

  @Test
  public void testGetType() {
    assertEquals(PrologTermType.ATOM, new PrologIntegerNumber(new BigInteger("111")).getType());
    assertEquals(PrologTermType.ATOM, new PrologIntegerNumber("24324324").getType());
  }

  @Test
  public void testGetText() {
    assertEquals("9879823432", new PrologIntegerNumber(9879823432L).getText());
    assertEquals("-2342343243", new PrologIntegerNumber(-2342343243L).getText());
    assertEquals("-23", new PrologIntegerNumber("-0000023").getText());
    assertEquals("123", new PrologIntegerNumber("0000000000000000000000000123").getText());
  }

  @Test
  public void testGetPriority() {
    assertEquals(0, new PrologIntegerNumber("97239847324").getPrecedence());
    assertEquals(0, new PrologIntegerNumber(123L).getPrecedence());
  }
}
