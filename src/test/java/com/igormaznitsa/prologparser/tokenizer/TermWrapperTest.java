package com.igormaznitsa.prologparser.tokenizer;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.igormaznitsa.prologparser.terms.TermType;
import org.junit.jupiter.api.Test;

public class TermWrapperTest {

  final Op testWrapped = Op.make(300, OpAssoc.FX, "---");

  @Test
  public void testGetText() {
    assertEquals("---", testWrapped.getText());
  }

  @Test
  public void testGetPrecedence() {
    assertEquals(300, testWrapped.getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals("op(300, fx, '---').", testWrapped.toString());
  }

  @Test
  public void testGetType() {
    assertEquals(TermType.OPERATOR, testWrapped.getType());
  }

}
