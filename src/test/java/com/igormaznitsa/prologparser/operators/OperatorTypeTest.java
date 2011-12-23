package com.igormaznitsa.prologparser.operators;

import static org.junit.Assert.*;
import org.junit.Test;
import com.igormaznitsa.prologparser.AbstractPrologParserTest;

public class OperatorTypeTest extends AbstractPrologParserTest {

    @Test
    public void testGetForName() {
        assertSame(OperatorType.XF, OperatorType.getForName("xf"));
        assertSame(OperatorType.FX, OperatorType.getForName("fx"));
        assertSame(OperatorType.XFX, OperatorType.getForName("xfx"));
        assertSame(OperatorType.XFY, OperatorType.getForName("xfy"));
        assertSame(OperatorType.YF, OperatorType.getForName("yf"));
        assertSame(OperatorType.YFX, OperatorType.getForName("yfx"));
        assertNull(OperatorType.getForName("yfy"));
    }

    @Test
    public void testGetText() {
        assertEquals("xf", OperatorType.XF.getText());
        assertEquals("fx", OperatorType.FX.getText());
        assertEquals("xfx", OperatorType.XFX.getText());
        assertEquals("xfy", OperatorType.XFY.getText());
        assertEquals("yf", OperatorType.YF.getText());
        assertEquals("yfx", OperatorType.YFX.getText());
    }
}
