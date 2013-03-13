package com.igormaznitsa.prologparser;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorType;

public class PrologTermWrapperTest {

    final static Operator testWrapped = Operator.makeOperator(300, OperatorType.FX, "---");
    
    final static PrologTermWrapper testWrapper = new PrologTermWrapper();
    
    static{
      testWrapper.setWrappedTerm(testWrapped);
    }
    
    @Test
    public void testGetText() {
      assertEquals(testWrapped.getText(), testWrapper.getText());
    }

    @Test
    public void testGetPriority() {
        assertEquals(testWrapped.getPriority(), testWrapper.getPriority());
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
        assertSame(testWrapped, testWrapper.getWrappedTerm());
    }
}
