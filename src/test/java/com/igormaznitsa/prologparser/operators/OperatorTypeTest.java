package com.igormaznitsa.prologparser.operators;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class OperatorTypeTest {

  @Test
  public void testGetForName() {
    assertSame(OpType.XF, OpType.findForName("xf").get());
    assertSame(OpType.FX, OpType.findForName("fx").get());
    assertSame(OpType.XFX, OpType.findForName("xfx").get());
    assertSame(OpType.XFY, OpType.findForName("xfy").get());
    assertSame(OpType.YF, OpType.findForName("yf").get());
    assertSame(OpType.YFX, OpType.findForName("yfx").get());
    assertFalse(OpType.findForName("yfy").isPresent());
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
