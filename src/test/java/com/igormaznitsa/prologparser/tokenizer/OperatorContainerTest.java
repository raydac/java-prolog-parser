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

import com.igormaznitsa.prologparser.terms.InternalSpecialCompoundTerm;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.TermType;
import org.junit.jupiter.api.Test;

import static com.igormaznitsa.prologparser.terms.OpContainer.make;
import static org.junit.jupiter.api.Assertions.*;

public class OperatorContainerTest {

  @Test
  public void testGetPrecedence() {
    assertEquals(make(Op.make(1000, OpAssoc.FX,
            "<>")).getPrecedence(), 0);
  }

  @Test
  public void testToString() {
    final Op operator = Op.make(100, OpAssoc.FX, "<>");
    final OpContainer container = make(operator);
    container.add(Op.make(300, OpAssoc.XFX, "<>"));
    container.add(Op.make(800, OpAssoc.YF, "<>"));
    assertEquals(
            "OpContainer [op(100, fx, '<>'). op(800, yf, '<>'). op(300, xfx, '<>').]",
            container.toString());
  }

  @Test
  public void testGetType() {
    final OpContainer cont = make(
            Op.make(1000, OpAssoc.FX, "<>"));
    assertEquals(TermType.OPERATOR, cont.getType());
    assertTrue(cont instanceof InternalSpecialCompoundTerm);
  }

  @Test
  public void testOperatorContainer() {
    assertThrows(NullPointerException.class, () -> make(null));

    final Op operator = Op.make(100, OpAssoc.FX, "<>");
    final OpContainer container = make(operator);
    assertEquals(1, container.size());
    assertSame(operator, container.getIfSingle());
  }

  @Test
  public void testAddOperator() {
    final Op operatorFX = Op.make(100, OpAssoc.FX, "<>");
    final Op operatorXFX = Op.make(400, OpAssoc.XFX, "<>");
    final Op operatorYF = Op.make(300, OpAssoc.YF, "<>");
    final Op otheroperatorYF = Op.make(300, OpAssoc.YF,
            "><");

    final OpContainer container = make(operatorFX);

    assertThrows(NullPointerException.class, () -> make(null));
    assertThrows(IllegalArgumentException.class, () -> container.add(otheroperatorYF));

    assertEquals(1, container.size());
    assertTrue(container.add(operatorXFX));
    assertEquals(2, container.size());
    assertTrue(container.add(operatorYF));
    assertEquals(3, container.size());

    assertFalse(container.add(operatorXFX));
    assertFalse(container.add(operatorFX));
    assertFalse(container.add(operatorYF));

    assertSame(operatorFX, container.findForType(OpAssoc.FX));
    assertSame(operatorXFX, container.findForType(OpAssoc.XFX));
    assertSame(operatorYF, container.findForType(OpAssoc.YF));
  }

  @Test
  public void testRemoveAll() {
    final Op operatorFX = Op.make(100, OpAssoc.FX, "<>");
    final Op operatorXFX = Op.make(400, OpAssoc.XFX, "<>");
    final Op operatorYF = Op.make(300, OpAssoc.YF, "<>");

    final OpContainer container = make(operatorFX);

    assertThrows(NullPointerException.class, () -> container.add(null));

    assertTrue(container.add(operatorXFX));
    assertTrue(container.add(operatorYF));

    assertEquals(3, container.size());
    container.clear();
    assertEquals(0, container.size());
    assertNull(container.findForType(OpAssoc.FX));
    assertNull(container.findForType(OpAssoc.XFX));
    assertNull(container.findForType(OpAssoc.YF));
  }

  @Test
  public void testRemove() {
    final Op operatorFX = Op.make(100, OpAssoc.FX, "<>");
    final Op operatorXFX = Op.make(400, OpAssoc.XFX, "<>");
    final Op operatorYF = Op.make(300, OpAssoc.YF, "<>");
    final Op otheroperatorXFX = Op.make(400, OpAssoc.XFX,
            "><");

    final OpContainer container = make(operatorFX);
    container.add(operatorXFX);
    container.add(operatorYF);

    assertEquals(3, container.size());

    assertTrue(container.remove(operatorFX));
    assertEquals(2, container.size());
    assertNull(container.findSimilar(OpAssoc.FX));

    assertThrows(NullPointerException.class, () -> container.remove(null));
    assertThrows(IllegalArgumentException.class, () -> container.remove(otheroperatorXFX));

    assertTrue(container.remove(operatorXFX));
    assertEquals(1, container.size());
    assertNull(container.findSimilar(OpAssoc.XFX));

    assertTrue(container.remove(operatorYF));
    assertEquals(0, container.size());
    assertNull(container.findSimilar(OpAssoc.YF));
  }

  @Test
  public void testSize() {
    final Op operatorFX = Op.make(100, OpAssoc.FX, "<>");
    final Op operatorXFX = Op.make(400, OpAssoc.XFX, "<>");
    final Op operatorYF = Op.make(300, OpAssoc.YF, "<>");

    final OpContainer container = make(operatorFX);

    assertEquals(1, container.size());
    container.clear();
    assertEquals(0, container.size());
    container.add(operatorFX);
    assertEquals(1, container.size());
    container.add(operatorXFX);
    assertEquals(2, container.size());
    container.add(operatorYF);
    assertEquals(3, container.size());
  }

  @Test
  public void testGetOperatorIfSingle() {
    final Op operatorFX = Op.make(100, OpAssoc.FX, "<>");
    final Op operatorXFX = Op.make(400, OpAssoc.XFX, "<>");
    final Op operatorYF = Op.make(300, OpAssoc.YF, "<>");

    final OpContainer container = make(operatorFX);

    assertSame(operatorFX, container.getIfSingle());
    container.add(operatorYF);
    assertNull(container.getIfSingle());
    container.add(operatorXFX);
    assertNull(container.getIfSingle());
    container.clear();
    assertNull(container.getIfSingle());

    container.add(operatorYF);
    assertEquals(operatorYF, container.getIfSingle());
  }

  @Test
  public void testFindCompatibleOperator() {
    final Op operatorFX = Op.make(100, OpAssoc.FX, "<>");
    final Op operatorXFX = Op.make(400, OpAssoc.XFX, "<>");
    final Op operatorYF = Op.make(300, OpAssoc.YF, "<>");

    final OpContainer container = make(operatorFX);

    container.add(operatorXFX);
    container.add(operatorYF);

    assertSame(operatorFX, container.findSimilar(false, true));
    assertSame(operatorXFX, container.findSimilar(true, true));
    assertSame(operatorYF, container.findSimilar(true, false));
    assertNull(container.findSimilar(false, false));

    assertTrue(container.remove(operatorXFX));

    assertSame(operatorFX, container.findSimilar(false, true));
    assertSame(operatorFX, container.findSimilar(true, true));
    assertSame(operatorYF, container.findSimilar(true, false));
    assertNull(container.findSimilar(false, false));

    assertTrue(container.remove(operatorFX));

    assertSame(operatorYF, container.findSimilar(false, true));
    assertSame(operatorYF, container.findSimilar(true, true));
    assertSame(operatorYF, container.findSimilar(true, false));
    assertNull(container.findSimilar(false, false));

    assertTrue(container.remove(operatorYF));

    assertNull(container.findSimilar(false, true));
    assertNull(container.findSimilar(true, true));
    assertNull(container.findSimilar(true, false));
    assertNull(container.findSimilar(false, false));
  }

  @Test
  public void testGetOperatorForType() {
    final Op operatorFX = Op.make(100, OpAssoc.FX, "<>");
    final Op operatorXFX = Op.make(400, OpAssoc.XFX, "<>");
    final Op operatorYF = Op.make(300, OpAssoc.YF, "<>");

    final OpContainer container = make(operatorFX);

    assertThrows(NullPointerException.class, () -> container.findForType(null));

    assertNull(container.findForType(OpAssoc.XFX));
    assertNull(container.findForType(OpAssoc.YF));

    container.add(operatorXFX);
    container.add(operatorYF);

    assertSame(operatorFX, container.findForType(OpAssoc.FX));
    assertSame(operatorXFX, container.findForType(OpAssoc.XFX));
    assertSame(operatorYF, container.findForType(OpAssoc.YF));
    assertNull(container.findForType(OpAssoc.YFX));
    assertNull(container.findForType(OpAssoc.XF));
  }

  @Test
  public void testGetOperatorForSimilarType() {
    final Op operatorFX = Op.make(100, OpAssoc.FX, "<>");
    final Op operatorXFX = Op.make(400, OpAssoc.XFX, "<>");
    final Op operatorYF = Op.make(300, OpAssoc.YF, "<>");

    final OpContainer container = make(operatorFX);
    container.add(operatorXFX);
    container.add(operatorYF);

    assertThrows(NullPointerException.class, () -> container.findSimilar(null));

    assertSame(operatorFX,
            container.findSimilar(OpAssoc.FY));
    assertSame(operatorXFX,
            container.findSimilar(OpAssoc.YFX));
    assertTrue(container.removeForType(OpAssoc.FX));
    assertNull(container.findSimilar(OpAssoc.FY));

  }

  @Test
  public void testRemoveOperatorForType() {
    final Op operatorFX = Op.make(100, OpAssoc.FX, "<>");
    final Op operatorXFX = Op.make(400, OpAssoc.XFX, "<>");
    final Op operatorYF = Op.make(300, OpAssoc.YF, "<>");

    final OpContainer container = make(operatorFX);
    container.add(operatorXFX);
    container.add(operatorYF);

    assertThrows(NullPointerException.class, () -> container.removeForType(null));

    assertTrue(container.removeForType(OpAssoc.FX));
    assertNull(container.findForType(OpAssoc.FX));
    assertTrue(container.removeForType(OpAssoc.XFX));
    assertNull(container.findForType(OpAssoc.XFX));
    assertTrue(container.removeForType(OpAssoc.YF));
    assertNull(container.findForType(OpAssoc.YF));
    assertFalse(container.removeForType(OpAssoc.YFX));
  }

  @Test
  public void testGetText() {
    assertEquals("<>", make(Op.make(1000,
            OpAssoc.FX, "<>")).getText());
  }
}
