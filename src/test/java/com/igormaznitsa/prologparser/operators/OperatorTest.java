/* 
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import org.junit.Test;
import org.mockito.Mockito;

import static org.junit.Assert.*;

public class OperatorTest extends AbstractPrologParserTest {

    @Test
    public void testGetPriority() {
        final Operator op = Operator.makeOperator(243, OperatorType.FX, "<>");
        assertEquals(op.getPriority(), 243);
    }

    @Test
    public void testToString() {
        assertEquals("op(231,xfy,'<>').", Operator.makeOperator(231, OperatorType.XFY,
                "<>").toString());
        assertEquals("op(100,fy,'><').", Operator.makeOperator(100, OperatorType.FY,
                "><").toString());
    }

    @Test
    public void testGetType() {
        final Operator op = Operator.makeOperator(243, OperatorType.FX, "<>");
        final Operator op2 = Operator.makeOperator(243, OperatorType.XFX, "><");
        assertEquals(PrologTermType.OPERATOR, op.getType());
        assertEquals(PrologTermType.OPERATOR, op2.getType());
    }

    @Test
    public void testMakeOperators() {
        final String[] names = new String[]{"op1", "op2", "op3", "op4"};
        try {
            Operator.makeOperators(Operator.PRIORITY_MAX - 1, OperatorType.FX,
                    names);
            fail("Must throw IAE for too low priority");
        } catch (IllegalArgumentException ex) {
        }

        try {
            Operator.makeOperators(Operator.PRIORITY_MIN + 1, OperatorType.FX,
                    names);
            fail("Must throw IAE for outbound priority");
        } catch (IllegalArgumentException ex) {
        }

        try {
            Operator.makeOperators(345, null, names);
            fail("Must throw NPE for null type");
        } catch (NullPointerException ex) {
        }

        try {
            Operator.makeOperators(345, OperatorType.FX, null);
            fail("Must throw NPE for null names");
        } catch (NullPointerException ex) {
        }

        final Operator[] operators = Operator.makeOperators(321,
                OperatorType.XFX, names);
        assertEquals(names.length, operators.length);

        for (int li = 0; li < names.length; li++) {
            final Operator op = operators[li];
            assertNotNull(op);
            assertEquals(names[li], op.getText());
            assertEquals(321, op.getPriority());
            assertEquals(OperatorType.XFX, op.getOperatorType());
        }
    }

    @Test
    public void testOperatorIntOperatorTypeString() {
        try {
            Operator.makeOperator(-1, OperatorType.FX, "<>");
            fail("Must throw IAE for negative priority");
        } catch (IllegalArgumentException ex) {
        }

        try {
            Operator.makeOperator(1201, OperatorType.FX, "<>");
            fail("Must throw IAE for outbound priority");
        } catch (IllegalArgumentException ex) {
        }

        try {
            Operator.makeOperator(333, OperatorType.FX, null);
            fail("Must throw NPE for null name");
        } catch (NullPointerException ex) {
        }

        try {
            Operator.makeOperator(333, null, "<>");
            fail("Must throw NPE for null type");
        } catch (NullPointerException ex) {
        }

        try {
            Operator.makeOperator(333, OperatorType.FX, "Hello");
            fail("Must throw IAE for capital first letter");
        } catch (IllegalArgumentException ex) {
        }

        try {
            Operator.makeOperator(333, OperatorType.FX, " <>");
            fail("Must throw IAE for space as the first letter");
        } catch (IllegalArgumentException ex) {
        }

        try {
            Operator.makeOperator(333, OperatorType.FX, "");
            fail("Must throw IAE for empty name");
        } catch (IllegalArgumentException ex) {
        }

        try {
            Operator.makeOperator(333, OperatorType.FX, "_hello");
            fail("Must throw IAE for '_' as the first letter");
        } catch (IllegalArgumentException ex) {
        }

        final Operator operator = Operator.makeOperator(100, OperatorType.XFY, "<>");
        assertEquals(100, operator.getPriority());
        assertEquals(OperatorType.XFY, operator.getOperatorType());
        assertEquals("<>", operator.getText());
    }

    @Test
    public void testGetOperatorType() {
        final Operator op = Operator.makeOperator(243, OperatorType.FX, "<>");
        final Operator op2 = Operator.makeOperator(243, OperatorType.XFX, "><");
        assertEquals(OperatorType.FX, op.getOperatorType());
        assertEquals(OperatorType.XFX, op2.getOperatorType());
    }

    @Test
    public void testCompatibleWith() {
        final Operator opFX = Operator.makeOperator(100, OperatorType.FX, "><");
        final Operator opFY = Operator.makeOperator(200, OperatorType.FY, "><");
        final Operator opYFX = Operator.makeOperator(300, OperatorType.YFX, "><");
        final Operator opXFX = Operator.makeOperator(400, OperatorType.XFX, "><");
        final Operator opXFY = Operator.makeOperator(500, OperatorType.XFY, "><");
        final Operator opYF = Operator.makeOperator(600, OperatorType.YF, "><");
        final Operator opXF = Operator.makeOperator(700, OperatorType.XF, "><");

        final PrologStructure empty = new PrologStructure("empty");
        final PrologStructure one = new PrologStructure(new PrologAtom(
                "functor"),
                new AbstractPrologTerm[]{new PrologAtom("first")});
        final PrologStructure two = new PrologStructure(new PrologAtom(
                "functor"), new AbstractPrologTerm[]{new PrologAtom("first"),
                new PrologAtom("second")});

        assertFalse(opFX.compatibleWith(empty));
        assertFalse(opFY.compatibleWith(empty));
        assertFalse(opYFX.compatibleWith(empty));
        assertFalse(opXFX.compatibleWith(empty));
        assertFalse(opXFY.compatibleWith(empty));
        assertFalse(opYF.compatibleWith(empty));
        assertFalse(opXF.compatibleWith(empty));

        assertTrue(opFX.compatibleWith(one));
        assertTrue(opFY.compatibleWith(one));
        assertFalse(opYFX.compatibleWith(one));
        assertFalse(opXFX.compatibleWith(one));
        assertFalse(opXFY.compatibleWith(one));
        assertTrue(opYF.compatibleWith(one));
        assertTrue(opXF.compatibleWith(one));

        assertTrue(opFX.compatibleWith(two));
        assertTrue(opFY.compatibleWith(two));
        assertTrue(opYFX.compatibleWith(two));
        assertTrue(opXFX.compatibleWith(two));
        assertTrue(opXFY.compatibleWith(two));
        assertTrue(opYF.compatibleWith(two));
        assertTrue(opXF.compatibleWith(two));

        final PrologStructure nullElementStructure = Mockito.mock(PrologStructure.class);
        Mockito.when(nullElementStructure.getArity()).thenReturn(1).thenReturn(1).thenReturn(2).thenReturn(2).thenReturn(2).thenReturn(2).thenReturn(2).thenReturn(1).thenReturn(1);

        assertFalse(opFX.compatibleWith(nullElementStructure));
        assertFalse(opFY.compatibleWith(nullElementStructure));

        assertFalse(opYFX.compatibleWith(nullElementStructure));
        assertFalse(opXFX.compatibleWith(nullElementStructure));
        assertFalse(opXFY.compatibleWith(nullElementStructure));
        assertFalse(opXF.compatibleWith(nullElementStructure));
        assertFalse(opXF.compatibleWith(nullElementStructure));

        assertFalse(opXF.compatibleWith(nullElementStructure));
        assertFalse(opXF.compatibleWith(nullElementStructure));
    }

    @Test
    public void testEquals() {
        final Operator opFX = Operator.makeOperator(100, OperatorType.FX, "><");
        final Operator opFX2 = Operator.makeOperator(100, OperatorType.FX, "><");
        final Operator opFY = Operator.makeOperator(100, OperatorType.FX, ">*<");

        assertFalse(opFX.equals("><"));
        assertFalse(opFX.equals(null));
        assertTrue(opFX.equals(opFX));
        assertTrue(opFX.equals(opFX2));
        assertFalse(opFX.equals(opFY));
    }

    @Test
    public void testHashCode() {
        final Operator opFX = Operator.makeOperator(100, OperatorType.FX, "><");
        final Operator opFX2 = Operator.makeOperator(100, OperatorType.FX, "><");

        assertFalse("><".hashCode() == opFX.hashCode());
        assertEquals(opFX.hashCode(), opFX2.hashCode());

    }

    @Test
    public void testGetText() {
        assertEquals("<>", Operator.makeOperator(121, OperatorType.FX, "<>").getText());
        assertEquals("><", Operator.makeOperator(121, OperatorType.XFX, "><").getText());
    }
}
