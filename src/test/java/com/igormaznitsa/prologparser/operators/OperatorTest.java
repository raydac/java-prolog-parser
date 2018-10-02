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
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.*;

public class OperatorTest extends AbstractPrologParserTest {

  @Test
  public void testGetPriority() {
    final Operator op = Operator.makeOperator(243, OpType.FX, "<>");
    assertEquals(243, op.getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals("op(231,xfy,'<>').", Operator.makeOperator(231, OpType.XFY, "<>").toString());
    assertEquals("op(100,fy,'><').", Operator.makeOperator(100, OpType.FY, "><").toString());
  }

  @Test
  public void testGetType() {
    final Operator op = Operator.makeOperator(243, OpType.FX, "<>");
    final Operator op2 = Operator.makeOperator(243, OpType.XFX, "><");
    assertEquals(PrologTermType.OPERATOR, op.getType());
    assertEquals(PrologTermType.OPERATOR, op2.getType());
  }

  @Test
  public void testMakeOperators() {
    final String[] names = new String[] {"op1", "op2", "op3", "op4"};

    assertThrows(IllegalArgumentException.class, () -> Operator.makeOperators(Operator.PRECEDENCE_MAX - 1, OpType.FX, names));
    assertThrows(IllegalArgumentException.class, () -> Operator.makeOperators(Operator.PRECEDENCE_MIN + 1, OpType.FX, names));
    assertThrows(NullPointerException.class, () -> Operator.makeOperators(345, null, names));
    assertThrows(NullPointerException.class, () -> Operator.makeOperators(345, OpType.FX, null));

    final Operator[] operators = Operator.makeOperators(321, OpType.XFX, names);
    assertEquals(names.length, operators.length);

    for (int li = 0; li < names.length; li++) {
      final Operator op = operators[li];
      assertNotNull(op);
      assertEquals(names[li], op.getText());
      assertEquals(321, op.getPrecedence());
      assertEquals(OpType.XFX, op.getOperatorType());
    }
  }

  @Test
  public void testOperatorIntOperatorTypeString() {
    assertThrows(IllegalArgumentException.class, () -> Operator.makeOperator(-1, OpType.FX, "<>"));
    assertThrows(IllegalArgumentException.class, () -> Operator.makeOperator(1201, OpType.FX, "<>"));
    assertThrows(NullPointerException.class, () -> Operator.makeOperator(333, OpType.FX, null));
    assertThrows(NullPointerException.class, () -> Operator.makeOperator(333, null, "<>"));
    assertThrows(IllegalArgumentException.class, () -> Operator.makeOperator(333, OpType.FX, "Hello"));
    assertThrows(IllegalArgumentException.class, () -> Operator.makeOperator(333, OpType.FX, " <>"));
    assertThrows(IllegalArgumentException.class, () -> Operator.makeOperator(333, OpType.FX, ""));
    assertThrows(IllegalArgumentException.class, () -> Operator.makeOperator(333, OpType.FX, "_hello"));

    final Operator operator = Operator.makeOperator(100, OpType.XFY, "<>");
    assertEquals(100, operator.getPrecedence());
    assertEquals(OpType.XFY, operator.getOperatorType());
    assertEquals("<>", operator.getText());
  }

  @Test
  public void testGetOperatorType() {
    final Operator op = Operator.makeOperator(243, OpType.FX, "<>");
    final Operator op2 = Operator.makeOperator(243, OpType.XFX, "><");
    assertEquals(OpType.FX, op.getOperatorType());
    assertEquals(OpType.XFX, op2.getOperatorType());
  }

  @Test
  public void testCompatibleWith() {
    final Operator opFX = Operator.makeOperator(100, OpType.FX, "><");
    final Operator opFY = Operator.makeOperator(200, OpType.FY, "><");
    final Operator opYFX = Operator.makeOperator(300, OpType.YFX, "><");
    final Operator opXFX = Operator.makeOperator(400, OpType.XFX, "><");
    final Operator opXFY = Operator.makeOperator(500, OpType.XFY, "><");
    final Operator opYF = Operator.makeOperator(600, OpType.YF, "><");
    final Operator opXF = Operator.makeOperator(700, OpType.XF, "><");

    final PrologStructure empty = new PrologStructure("empty");
    final PrologStructure one = new PrologStructure(new PrologAtom(
        "functor"),
        new AbstractPrologTerm[] {new PrologAtom("first")});
    final PrologStructure two = new PrologStructure(new PrologAtom(
        "functor"), new AbstractPrologTerm[] {new PrologAtom("first"),
        new PrologAtom("second")});

    assertFalse(opFX.isCompatibleWith(empty));
    assertFalse(opFY.isCompatibleWith(empty));
    assertFalse(opYFX.isCompatibleWith(empty));
    assertFalse(opXFX.isCompatibleWith(empty));
    assertFalse(opXFY.isCompatibleWith(empty));
    assertFalse(opYF.isCompatibleWith(empty));
    assertFalse(opXF.isCompatibleWith(empty));

    assertTrue(opFX.isCompatibleWith(one));
    assertTrue(opFY.isCompatibleWith(one));
    assertFalse(opYFX.isCompatibleWith(one));
    assertFalse(opXFX.isCompatibleWith(one));
    assertFalse(opXFY.isCompatibleWith(one));
    assertTrue(opYF.isCompatibleWith(one));
    assertTrue(opXF.isCompatibleWith(one));

    assertTrue(opFX.isCompatibleWith(two));
    assertTrue(opFY.isCompatibleWith(two));
    assertTrue(opYFX.isCompatibleWith(two));
    assertTrue(opXFX.isCompatibleWith(two));
    assertTrue(opXFY.isCompatibleWith(two));
    assertTrue(opYF.isCompatibleWith(two));
    assertTrue(opXF.isCompatibleWith(two));

    final PrologStructure nullElementStructure = Mockito.mock(PrologStructure.class);
    Mockito.when(nullElementStructure.getArity()).thenReturn(1).thenReturn(1).thenReturn(2).thenReturn(2).thenReturn(2).thenReturn(2).thenReturn(2).thenReturn(1).thenReturn(1);

    assertFalse(opFX.isCompatibleWith(nullElementStructure));
    assertFalse(opFY.isCompatibleWith(nullElementStructure));

    assertFalse(opYFX.isCompatibleWith(nullElementStructure));
    assertFalse(opXFX.isCompatibleWith(nullElementStructure));
    assertFalse(opXFY.isCompatibleWith(nullElementStructure));
    assertFalse(opXF.isCompatibleWith(nullElementStructure));
    assertFalse(opXF.isCompatibleWith(nullElementStructure));

    assertFalse(opXF.isCompatibleWith(nullElementStructure));
    assertFalse(opXF.isCompatibleWith(nullElementStructure));
  }

  @Test
  public void testEquals() {
    final Operator opFX = Operator.makeOperator(100, OpType.FX, "><");
    final Operator opFX2 = Operator.makeOperator(100, OpType.FX, "><");
    final Operator opFY = Operator.makeOperator(100, OpType.FX, ">*<");

    assertFalse(opFX.equals("><"));
    assertFalse(opFX.equals(null));
    assertTrue(opFX.equals(opFX));
    assertTrue(opFX.equals(opFX2));
    assertFalse(opFX.equals(opFY));
  }

  @Test
  public void testHashCode() {
    final Operator opFX = Operator.makeOperator(100, OpType.FX, "><");
    final Operator opFX2 = Operator.makeOperator(100, OpType.FX, "><");

    assertFalse("><".hashCode() == opFX.hashCode());
    assertEquals(opFX.hashCode(), opFX2.hashCode());

  }

  @Test
  public void testGetText() {
    assertEquals("<>", Operator.makeOperator(121, OpType.FX, "<>").getText());
    assertEquals("><", Operator.makeOperator(121, OpType.XFX, "><").getText());
  }
}
