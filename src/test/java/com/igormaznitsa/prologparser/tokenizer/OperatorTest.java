/*
 * Copyright (c) 2011-2018 Igor Maznitsa. All rights reserved.
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package com.igormaznitsa.prologparser.tokenizer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

@SuppressWarnings({"AssertBetweenInconvertibleTypes", "EqualsWithItself"})
class OperatorTest {

  @Test
  void testGetPrecedence() {
    final Op op = Op.make(243, OpAssoc.FX, "<>");
    assertEquals(243, op.getPrecedence());
  }

  @Test
  void testToString() {
    assertEquals("op(231, xfy, '<>').", Op.make(231, OpAssoc.XFY, "<>").toString());
    assertEquals("op(100, fy, '><').", Op.make(100, OpAssoc.FY, "><").toString());
  }

  @Test
  void testGetType() {
    final Op op = Op.make(243, OpAssoc.FX, "<>");
    final Op op2 = Op.make(243, OpAssoc.XFX, "><");
    assertEquals(TermType.OPERATOR, op.getType());
    assertEquals(TermType.OPERATOR, op2.getType());
  }

  @Test
  void testMakeOperators() {
    final String[] names = new String[] {"op1", "op2", "op3", "op4"};

    assertThrows(IllegalArgumentException.class,
        () -> Op.make(Op.PRECEDENCE_MAX - 1, OpAssoc.FX, names));
    assertThrows(IllegalArgumentException.class,
        () -> Op.make(Op.PRECEDENCE_MIN + 1, OpAssoc.FX, names));
    assertThrows(NullPointerException.class, () -> Op.make(345, null, names));
    assertThrows(NullPointerException.class, () -> Op.make(345, OpAssoc.FX, (String[]) null));

    final Op operators = Op.make(321, OpAssoc.XFX, names);
    assertEquals(names.length, operators.streamOp().count());
  }

  @Test
  void testOperatorIntOperatorTypeString() {
    assertThrows(IllegalArgumentException.class, () -> Op.make(-1, OpAssoc.FX, "<>"));
    assertThrows(IllegalArgumentException.class, () -> Op.make(1201, OpAssoc.FX, "<>"));
    assertThrows(NullPointerException.class, () -> Op.make(333, OpAssoc.FX, (String[]) null));
    assertThrows(NullPointerException.class, () -> Op.make(333, null, "<>"));
    assertThrows(IllegalArgumentException.class, () -> Op.make(333, OpAssoc.FX, "Hello"));
    assertThrows(IllegalArgumentException.class, () -> Op.make(333, OpAssoc.FX, " <>"));
    assertThrows(IllegalArgumentException.class, () -> Op.make(333, OpAssoc.FX, ""));
    assertThrows(IllegalArgumentException.class, () -> Op.make(333, OpAssoc.FX, "_hello"));

    final Op operator = Op.make(100, OpAssoc.XFY, "<>");
    assertEquals(100, operator.getPrecedence());
    assertEquals(OpAssoc.XFY, operator.getAssoc());
    assertEquals("<>", operator.getText());
  }

  @Test
  void testGetOperatorType() {
    final Op op = Op.make(243, OpAssoc.FX, "<>");
    final Op op2 = Op.make(243, OpAssoc.XFX, "><");
    assertEquals(OpAssoc.FX, op.getAssoc());
    assertEquals(OpAssoc.XFX, op2.getAssoc());
  }

  @Test
  void testCompatibleWith() {
    final Op opFX = Op.make(100, OpAssoc.FX, "><");
    final Op opFY = Op.make(200, OpAssoc.FY, "><");
    final Op opYFX = Op.make(300, OpAssoc.YFX, "><");
    final Op opXFX = Op.make(400, OpAssoc.XFX, "><");
    final Op opXFY = Op.make(500, OpAssoc.XFY, "><");
    final Op opYF = Op.make(600, OpAssoc.YF, "><");
    final Op opXF = Op.make(700, OpAssoc.XF, "><");

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
    Mockito.when(nullElementStructure.getArity()).thenReturn(1).thenReturn(1).thenReturn(2)
        .thenReturn(2).thenReturn(2).thenReturn(2).thenReturn(2).thenReturn(1).thenReturn(1);

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
  void testEquals() {
    final Op opFX = Op.make(100, OpAssoc.FX, "><");
    final Op opFX2 = Op.make(100, OpAssoc.FX, "><");
    final Op opFY = Op.make(100, OpAssoc.FX, ">*<");

    assertNotEquals("><", opFX);
    assertNotEquals(null, opFX);
    assertEquals(opFX, opFX);
    assertEquals(opFX, opFX2);
    assertNotEquals(opFX, opFY);
  }

  @Test
  void testHashCode() {
    final Op opFX = Op.make(100, OpAssoc.FX, "><");
    final Op opFX2 = Op.make(100, OpAssoc.FX, "><");

    assertNotEquals("><".hashCode(), opFX.hashCode());
    assertEquals(opFX.hashCode(), opFX2.hashCode());

  }

  @Test
  void testGetText() {
    assertEquals("<>", Op.make(121, OpAssoc.FX, "<>").getText());
    assertEquals("><", Op.make(121, OpAssoc.XFX, "><").getText());
  }
}
