package com.igormaznitsa.prologparser.terms;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class QuotationTest {

  @Test
  public void testFormatString() {
    assertEquals("None", Quotation.NONE.formatString("None"));
    assertEquals("'None'", Quotation.SINGLE.formatString("None"));
    assertEquals("\"None\"", Quotation.DOUBLE.formatString("None"));
    assertEquals("`None`", Quotation.BACK_TICK.formatString("None"));
    assertEquals("%None", Quotation.COMMENT_LINE.formatString("None"));
    assertEquals("/*None*/", Quotation.COMMENT_BLOCK.formatString("None"));
  }
}