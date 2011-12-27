package com.igormaznitsa.prologparser;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorType;

public class PrologTermWrapperTest {

    final Operator testWrapped = Operator.makeOperator(300, OperatorType.FX, "---");

    @Test
    public void testGetText() {
        assertEquals(testWrapped.getText(), new PrologTermWrapper(testWrapped).getText());
    }

    @Test
    public void testGetPriority() {
        assertEquals(testWrapped.getPriority(), new PrologTermWrapper(testWrapped).getPriority());
    }

    @Test
    public void testToString() {
        assertEquals(testWrapped.toString(), new PrologTermWrapper(testWrapped).toString());
    }

    @Test
    public void testGetType() {
        assertEquals(testWrapped.getType(), new PrologTermWrapper(testWrapped).getType());
    }

    @Test
    public void testPrologTermWrapperAbstractPrologTerm() {
        try {
            new PrologTermWrapper(null);
            fail("Must throw NPE for null wrapped object");
        } catch (NullPointerException ex) {
        }

        new PrologTermWrapper(testWrapped);
    }

    @Test
    public void testGetWrappedTerm() {
        assertSame(testWrapped, new PrologTermWrapper(testWrapped).getWrappedTerm());
    }
}
