package com.igormaznitsa.prologparser.operators;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class OperatorTypeTest {

  @Test
  public void testGetForName() {
    assertSame(OpType.XF, OpType.getForName("xf").get());
    assertSame(OpType.FX, OpType.getForName("fx").get());
    assertSame(OpType.XFX, OpType.getForName("xfx").get());
    assertSame(OpType.XFY, OpType.getForName("xfy").get());
    assertSame(OpType.YF, OpType.getForName("yf").get());
    assertSame(OpType.YFX, OpType.getForName("yfx").get());
    assertFalse(OpType.getForName("yfy").isPresent());
  }

  @Test
  public void testGetText() {
    assertEquals("xf", OpType.XF.getText());
    assertEquals("fx", OpType.FX.getText());
    assertEquals("xfx", OpType.XFX.getText());
    assertEquals("xfy", OpType.XFY.getText());
    assertEquals("yf", OpType.YF.getText());
    assertEquals("yfx", OpType.YFX.getText());
  }
}
