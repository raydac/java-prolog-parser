package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.operators.Op;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

public class TermWrapperTest {

  final static Op testWrapped = Op.makeOne(300, OpType.FX, "---");

  final static TermWrapper testWrapper = new TermWrapper(testWrapped);

  @Test
  public void testGetText() {
    assertEquals(testWrapped.getText(), testWrapper.getText());
  }

  @Test
  public void testGetPriority() {
    assertEquals(testWrapped.getPrecedence(), testWrapper.getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals(testWrapped.toString(), testWrapper.toString());
  }

  @Test
  public void testGetType() {
    assertEquals(testWrapped.getType(), testWrapper.getType());
  }

  @Test
  public void testGetWrappedTerm() {
    assertSame(testWrapped, testWrapper.getTerm());
  }
}
