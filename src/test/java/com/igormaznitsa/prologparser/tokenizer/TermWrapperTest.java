package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.utils.SoftObjectPool;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

public class TermWrapperTest {

  final Op testWrapped = Op.make(300, OpAssoc.FX, "---");
  final SoftObjectPool<TermWrapper> pool = new SoftObjectPool<TermWrapper>(32) {
    @Override
    public final TermWrapper get() {
      return new TermWrapper(this).setWrappedTerm(testWrapped).setWrappedTerm(testWrapped);
    }
  };

  @Test
  public void testGetText() {
    assertEquals(testWrapped.getTermText(), pool.find().getTermText());
  }

  @Test
  public void testGetPrecedence() {
    assertEquals(testWrapped.getPrecedence(), pool.find().getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals(testWrapped.toString(), pool.find().toString());
  }

  @Test
  public void testGetType() {
    assertEquals(testWrapped.getTermType(), pool.find().getTermType());
  }

  @Test
  public void testGetWrappedTerm() {
    assertSame(testWrapped, pool.find().getWrappedTerm());
  }
}
