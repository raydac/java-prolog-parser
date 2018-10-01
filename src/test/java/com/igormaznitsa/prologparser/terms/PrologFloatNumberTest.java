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

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class PrologFloatNumberTest extends AbstractPrologParserTest {

  @Test
  public void testToString() {
    assertEquals("0.0", new PrologFloatNumber(-0.0d).toString());
    assertEquals("0.0", new PrologFloatNumber("-0.0").toString());
    assertEquals("-21.0", new PrologFloatNumber(-21.0d).toString());
    assertEquals("231.2218979873946", new PrologFloatNumber("231.2218979873946").toString());
  }

  @Test
  public void testNeg() {
    assertEquals(345.223d, ((PrologFloatNumber) new PrologFloatNumber(
        -345.223d).neg()).getValue().doubleValue(), Double.MIN_NORMAL);
    assertEquals(-0.0003d, ((PrologFloatNumber) new PrologFloatNumber(
        0.0003d).neg()).getValue().doubleValue(), Double.MIN_NORMAL);
  }

  @Test
  public void testGetValue() {
    final BigDecimal test1 = new BigDecimal("0.0000000000007623723674621836817263437862876430000234234234362487238426123213324321432432000234324123213");
    final BigDecimal test2 = new BigDecimal("-8923749873294261283192830981284039284981273982173.893249827398213092183092498327948217039821038120302432094");

    assertEquals(test1, new PrologFloatNumber(test1).getValue());
    assertEquals(test2, new PrologFloatNumber(test2).getValue());
  }

  @Test
  public void testGetType() {
    assertEquals(PrologTermType.ATOM,
        new PrologFloatNumber(234.23d).getType());
    assertEquals(PrologTermType.ATOM,
        new PrologFloatNumber("234").getType());
  }

  @Test
  public void testGetText() {
    assertEquals("234.2329834912938", new PrologFloatNumber("234.2329834912938").getText());
    assertEquals("-0.00000242324324", new PrologFloatNumber("-00000.00000242324324").getText());
    assertEquals("0.00000", new PrologFloatNumber("00000.00000").getText());
    assertEquals("0.0", new PrologFloatNumber("0").getText());
    assertEquals("123.0", new PrologFloatNumber("123").getText());
    assertEquals("0.123", new PrologFloatNumber(".123").getText());
    assertEquals("0.00123", new PrologFloatNumber(".00123").getText());
  }

  @Test
  public void testGetPriority() {
    assertEquals(0, new PrologFloatNumber(23412213.002131d).getPrecedence());
    assertEquals(0, new PrologFloatNumber(-000.002131d).getPrecedence());
  }

  @Test
  public void testPrologFloatNumberString() {
    assertEquals("0.1234221179298734", new PrologFloatNumber("0.123422117929873388").getText());
    assertEquals("-2.987234987239848", new PrologFloatNumber("-2.9872349872398479").getText());

    assertThrows(NullPointerException.class, () -> new PrologFloatNumber((String) null));
    assertThrows(NumberFormatException.class, () -> new PrologFloatNumber("wrong number"));
  }

  @Test
  public void testPrologFloatNumberDouble() {
    assertEquals(0.1234221d, new PrologFloatNumber(0.1234221d).getValue().doubleValue(), Double.MIN_NORMAL);
    assertEquals(-0.00021234221d, new PrologFloatNumber(-0.00021234221d).getValue().doubleValue(), Double.MIN_NORMAL);
  }

  @Test
  public void testPrologFloatNumberBigDecimal() {
    assertThrows(NullPointerException.class, () -> new PrologFloatNumber((BigDecimal) null));

    final BigDecimal test1 = new BigDecimal("0.0000000000007623723674621836817263437862876430000234234234362487238426123213324321432432000234324123213");
    final BigDecimal test2 = new BigDecimal("-8923749873294261283192830981284039284981273982173.893249827398213092183092498327948217039821038120302432094");

    assertEquals(test1, new PrologFloatNumber(test1).getValue());
    assertEquals(test2, new PrologFloatNumber(test2).getValue());
  }

  @Test
  public void testPrologFloatNumberStringIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologFloatNumber((String) null, 0, 0));

    final AbstractPrologTerm term = new PrologFloatNumber("123.0", 1, 2);
    assertEquals(1, term.getStrPosition());
    assertEquals(2, term.getLineNumber());
  }

  @Test
  public void testPrologFloatNumberDoubleIntInt() {
    final AbstractPrologTerm term = new PrologFloatNumber(123.0d, 1, 2);
    assertEquals(1, term.getStrPosition());
    assertEquals(2, term.getLineNumber());
  }

  @Test
  public void testPrologFloatNumberBigDecimalIntInt() {
    try {
      new PrologFloatNumber((BigDecimal) null, 1, 2);
    } catch (NullPointerException ex) {
    }

    final AbstractPrologTerm term = new PrologFloatNumber(BigDecimal.ONE, 1, 2);
    assertEquals(1, term.getStrPosition());
    assertEquals(2, term.getLineNumber());
  }
}
