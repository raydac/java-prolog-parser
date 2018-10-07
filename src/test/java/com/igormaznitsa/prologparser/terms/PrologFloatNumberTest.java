package com.igormaznitsa.prologparser.terms;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class PrologFloatNumberTest {

  @Test
  public void testToString() {
    assertEquals("0.0", new PrologFloat(-0.0d).toString());
    assertEquals("0.0", new PrologFloat("-0.0").toString());
    assertEquals("-21.0", new PrologFloat(-21.0d).toString());
    assertEquals("231.2218979873946", new PrologFloat("231.2218979873946").toString());
  }

  @Test
  public void testNeg() {
    assertEquals(345.223d, ((PrologFloat) new PrologFloat(
        -345.223d).neg()).getNumber().doubleValue(), Double.MIN_NORMAL);
    assertEquals(-0.0003d, ((PrologFloat) new PrologFloat(
        0.0003d).neg()).getNumber().doubleValue(), Double.MIN_NORMAL);
  }

  @Test
  public void testGetValue() {
    final BigDecimal test1 = new BigDecimal("0.0000000000007623723674621836817263437862876430000234234234362487238426123213324321432432000234324123213");
    final BigDecimal test2 = new BigDecimal("-8923749873294261283192830981284039284981273982173.893249827398213092183092498327948217039821038120302432094");

    assertEquals(test1, new PrologFloat(test1).getFloatValue());
    assertEquals(test2, new PrologFloat(test2).getFloatValue());
  }

  @Test
  public void testGetType() {
    assertEquals(TermType.ATOM,
        new PrologFloat(234.23d).getTermType());
    assertEquals(TermType.ATOM,
        new PrologFloat("234").getTermType());
  }

  @Test
  public void testGetText() {
    assertEquals("234.2329834912938", new PrologFloat("234.2329834912938").getTermText());
    assertEquals("-0.00000242324324", new PrologFloat("-00000.00000242324324").getTermText());
    assertEquals("0.00000", new PrologFloat("00000.00000").getTermText());
    assertEquals("0.0", new PrologFloat("0").getTermText());
    assertEquals("123.0", new PrologFloat("123").getTermText());
    assertEquals("0.123", new PrologFloat(".123").getTermText());
    assertEquals("0.00123", new PrologFloat(".00123").getTermText());
  }

  @Test
  public void testGetPriority() {
    assertEquals(0, new PrologFloat(23412213.002131d).getPrecedence());
    assertEquals(0, new PrologFloat(-000.002131d).getPrecedence());
  }

  @Test
  public void testPrologFloatNumberString() {
    assertEquals("0.1234221179298734", new PrologFloat("0.123422117929873388").getTermText());
    assertEquals("-2.987234987239848", new PrologFloat("-2.9872349872398479").getTermText());

    assertThrows(NullPointerException.class, () -> new PrologFloat((String) null));
    assertThrows(NumberFormatException.class, () -> new PrologFloat("wrong number"));
  }

  @Test
  public void testPrologFloatNumberDouble() {
    assertEquals(0.1234221d, new PrologFloat(0.1234221d).getNumber().doubleValue(), Double.MIN_NORMAL);
    assertEquals(-0.00021234221d, new PrologFloat(-0.00021234221d).getNumber().doubleValue(), Double.MIN_NORMAL);
  }

  @Test
  public void testPrologFloatNumberBigDecimal() {
    assertThrows(NullPointerException.class, () -> new PrologFloat((BigDecimal) null));

    final BigDecimal test1 = new BigDecimal("0.0000000000007623723674621836817263437862876430000234234234362487238426123213324321432432000234324123213");
    final BigDecimal test2 = new BigDecimal("-8923749873294261283192830981284039284981273982173.893249827398213092183092498327948217039821038120302432094");

    assertEquals(test1, new PrologFloat(test1).getFloatValue());
    assertEquals(test2, new PrologFloat(test2).getFloatValue());
  }

  @Test
  public void testPrologFloatNumberStringIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologFloat((String) null, 0, 0));

    final PrologTerm term = new PrologFloat("123.0", 2, 1);
    assertEquals(1, term.getPos());
    assertEquals(2, term.getLine());
  }

  @Test
  public void testPrologFloatNumberDoubleIntInt() {
    final PrologTerm term = new PrologFloat(123.0d, 2, 1);
    assertEquals(1, term.getPos());
    assertEquals(2, term.getLine());
  }

  @Test
  public void testPrologFloatNumberBigDecimalIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologFloat((BigDecimal) null, 2, 1));

    final PrologTerm term = new PrologFloat(BigDecimal.ONE, 2, 1);
    assertEquals(1, term.getPos());
    assertEquals(2, term.getLine());
  }
}
