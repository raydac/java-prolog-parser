package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.operators.Op;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

public class TermWrapperTest {

  final Op testWrapped = Op.makeOne(300, OpType.FX, "---");
  final SoftObjectPool<TermWrapper> pool = new SoftObjectPool<TermWrapper>(32){
    @Override
    public final TermWrapper get() {
      return new TermWrapper(this).setTerm(testWrapped).setTerm(testWrapped);
    }
  };

  @Test
  public void testGetText() {
    assertEquals(testWrapped.getText(), pool.find().getText());
  }

  @Test
  public void testGetPriority() {
    assertEquals(testWrapped.getPrecedence(), pool.find().getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals(testWrapped.toString(), pool.find().toString());
  }

  @Test
  public void testGetType() {
    assertEquals(testWrapped.getType(), pool.find().getType());
  }

  @Test
  public void testGetWrappedTerm() {
    assertSame(testWrapped, pool.find().getTerm());
  }
}
