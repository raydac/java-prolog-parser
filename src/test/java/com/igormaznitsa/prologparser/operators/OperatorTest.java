package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.*;

public class OperatorTest {

  @Test
  public void testGetPriority() {
    final Op op = Op.makeOne(243, OpType.FX, "<>");
    assertEquals(243, op.getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals("of(231,xfy,'<>').", Op.makeOne(231, OpType.XFY, "<>").toString());
    assertEquals("of(100,fy,'><').", Op.makeOne(100, OpType.FY, "><").toString());
  }

  @Test
  public void testGetType() {
    final Op op = Op.makeOne(243, OpType.FX, "<>");
    final Op op2 = Op.makeOne(243, OpType.XFX, "><");
    assertEquals(TermType.__OPERATOR__, op.getTermType());
    assertEquals(TermType.__OPERATOR__, op2.getTermType());
  }

  @Test
  public void testMakeOperators() {
    final String[] names = new String[] {"op1", "op2", "op3", "op4"};

    assertThrows(IllegalArgumentException.class, () -> Op.make(Op.PRECEDENCE_MAX - 1, OpType.FX, names));
    assertThrows(IllegalArgumentException.class, () -> Op.make(Op.PRECEDENCE_MIN + 1, OpType.FX, names));
    assertThrows(NullPointerException.class, () -> Op.make(345, null, names));
    assertThrows(NullPointerException.class, () -> Op.make(345, OpType.FX, null));

    final Op[] operators = Op.make(321, OpType.XFX, names);
    assertEquals(names.length, operators.length);

    for (int li = 0; li < names.length; li++) {
      final Op op = operators[li];
      assertNotNull(op);
      assertEquals(names[li], op.getTermText());
      assertEquals(321, op.getPrecedence());
      assertEquals(OpType.XFX, op.getOpType());
    }
  }

  @Test
  public void testOperatorIntOperatorTypeString() {
    assertThrows(IllegalArgumentException.class, () -> Op.makeOne(-1, OpType.FX, "<>"));
    assertThrows(IllegalArgumentException.class, () -> Op.makeOne(1201, OpType.FX, "<>"));
    assertThrows(NullPointerException.class, () -> Op.makeOne(333, OpType.FX, null));
    assertThrows(NullPointerException.class, () -> Op.makeOne(333, null, "<>"));
    assertThrows(IllegalArgumentException.class, () -> Op.makeOne(333, OpType.FX, "Hello"));
    assertThrows(IllegalArgumentException.class, () -> Op.makeOne(333, OpType.FX, " <>"));
    assertThrows(IllegalArgumentException.class, () -> Op.makeOne(333, OpType.FX, ""));
    assertThrows(IllegalArgumentException.class, () -> Op.makeOne(333, OpType.FX, "_hello"));

    final Op operator = Op.makeOne(100, OpType.XFY, "<>");
    assertEquals(100, operator.getPrecedence());
    assertEquals(OpType.XFY, operator.getOpType());
    assertEquals("<>", operator.getTermText());
  }

  @Test
  public void testGetOperatorType() {
    final Op op = Op.makeOne(243, OpType.FX, "<>");
    final Op op2 = Op.makeOne(243, OpType.XFX, "><");
    assertEquals(OpType.FX, op.getOpType());
    assertEquals(OpType.XFX, op2.getOpType());
  }

  @Test
  public void testCompatibleWith() {
    final Op opFX = Op.makeOne(100, OpType.FX, "><");
    final Op opFY = Op.makeOne(200, OpType.FY, "><");
    final Op opYFX = Op.makeOne(300, OpType.YFX, "><");
    final Op opXFX = Op.makeOne(400, OpType.XFX, "><");
    final Op opXFY = Op.makeOne(500, OpType.XFY, "><");
    final Op opYF = Op.makeOne(600, OpType.YF, "><");
    final Op opXF = Op.makeOne(700, OpType.XF, "><");

    final PrologStruct empty = new PrologStruct("empty");
    final PrologStruct one = new PrologStruct(new PrologAtom(
        "functor"),
        new PrologTerm[] {new PrologAtom("first")});
    final PrologStruct two = new PrologStruct(new PrologAtom(
        "functor"), new PrologTerm[] {new PrologAtom("first"),
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

    final PrologStruct nullElementStructure = Mockito.mock(PrologStruct.class);
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
    final Op opFX = Op.makeOne(100, OpType.FX, "><");
    final Op opFX2 = Op.makeOne(100, OpType.FX, "><");
    final Op opFY = Op.makeOne(100, OpType.FX, ">*<");

    assertFalse(opFX.equals("><"));
    assertFalse(opFX.equals(null));
    assertTrue(opFX.equals(opFX));
    assertTrue(opFX.equals(opFX2));
    assertFalse(opFX.equals(opFY));
  }

  @Test
  public void testHashCode() {
    final Op opFX = Op.makeOne(100, OpType.FX, "><");
    final Op opFX2 = Op.makeOne(100, OpType.FX, "><");

    assertFalse("><".hashCode() == opFX.hashCode());
    assertEquals(opFX.hashCode(), opFX2.hashCode());

  }

  @Test
  public void testGetText() {
    assertEquals("<>", Op.makeOne(121, OpType.FX, "<>").getTermText());
    assertEquals("><", Op.makeOne(121, OpType.XFX, "><").getTermText());
  }
}
