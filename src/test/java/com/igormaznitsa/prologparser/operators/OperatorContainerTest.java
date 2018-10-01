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
import com.igormaznitsa.prologparser.terms.PrologTermType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class OperatorContainerTest extends AbstractPrologParserTest {

  @Test
  public void testGetPriority() {
    assertEquals(new OperatorContainer(Operator.makeOperator(1000, OperatorType.FX,
        "<>")).getPrecedence(), 0);
  }

  @Test
  public void testToString() {
    final Operator operator = Operator.makeOperator(100, OperatorType.FX, "<>");
    final OperatorContainer container = new OperatorContainer(operator);
    container.addOperator(Operator.makeOperator(300, OperatorType.XFX, "<>"));
    container.addOperator(Operator.makeOperator(800, OperatorType.YF, "<>"));
    assertEquals(
        "OperatorContainer [op(100,fx,'<>'). op(800,yf,'<>'). op(300,xfx,'<>').]",
        container.toString());
  }

  @Test
  public void testGetType() {
    assertEquals(PrologTermType.OPERATORS, new OperatorContainer(
        Operator.makeOperator(1000, OperatorType.FX, "<>")).getType());
  }

  @Test
  public void testOperatorContainer() {
    assertThrows(NullPointerException.class, () -> new OperatorContainer(null));

    final Operator operator = Operator.makeOperator(100, OperatorType.FX, "<>");
    final OperatorContainer container = new OperatorContainer(operator);
    assertEquals(1, container.size());
    assertSame(operator, container.getOperatorIfSingle());
  }

  @Test
  public void testAddOperator() {
    final Operator operatorFX = Operator.makeOperator(100, OperatorType.FX, "<>");
    final Operator operatorXFX = Operator.makeOperator(400, OperatorType.XFX, "<>");
    final Operator operatorYF = Operator.makeOperator(300, OperatorType.YF, "<>");
    final Operator otheroperatorYF = Operator.makeOperator(300, OperatorType.YF,
        "><");

    final OperatorContainer container = new OperatorContainer(operatorFX);

    assertThrows(NullPointerException.class, () -> container.addOperator(null));
    assertThrows(IllegalArgumentException.class, () -> container.addOperator(otheroperatorYF));

    assertEquals(1, container.size());
    assertTrue(container.addOperator(operatorXFX));
    assertEquals(2, container.size());
    assertTrue(container.addOperator(operatorYF));
    assertEquals(3, container.size());

    assertFalse(container.addOperator(operatorXFX));
    assertFalse(container.addOperator(operatorFX));
    assertFalse(container.addOperator(operatorYF));

    assertSame(operatorFX, container.getOperatorForType(OperatorType.FX));
    assertSame(operatorXFX, container.getOperatorForType(OperatorType.XFX));
    assertSame(operatorYF, container.getOperatorForType(OperatorType.YF));
  }

  @Test
  public void testRemoveAll() {
    final Operator operatorFX = Operator.makeOperator(100, OperatorType.FX, "<>");
    final Operator operatorXFX = Operator.makeOperator(400, OperatorType.XFX, "<>");
    final Operator operatorYF = Operator.makeOperator(300, OperatorType.YF, "<>");

    final OperatorContainer container = new OperatorContainer(operatorFX);

    assertThrows(NullPointerException.class, () -> container.addOperator(null));

    assertTrue(container.addOperator(operatorXFX));
    assertTrue(container.addOperator(operatorYF));

    assertEquals(3, container.size());
    container.removeAll();
    assertEquals(0, container.size());
    assertNull(container.getOperatorForType(OperatorType.FX));
    assertNull(container.getOperatorForType(OperatorType.XFX));
    assertNull(container.getOperatorForType(OperatorType.YF));
  }

  @Test
  public void testRemove() {
    final Operator operatorFX = Operator.makeOperator(100, OperatorType.FX, "<>");
    final Operator operatorXFX = Operator.makeOperator(400, OperatorType.XFX, "<>");
    final Operator operatorYF = Operator.makeOperator(300, OperatorType.YF, "<>");
    final Operator otheroperatorXFX = Operator.makeOperator(400, OperatorType.XFX,
        "><");

    final OperatorContainer container = new OperatorContainer(operatorFX);
    container.addOperator(operatorXFX);
    container.addOperator(operatorYF);

    assertEquals(3, container.size());

    assertTrue(container.remove(operatorFX));
    assertEquals(2, container.size());
    assertNull(container.getOperatorForSimilarType(OperatorType.FX));

    assertThrows(NullPointerException.class, () -> container.remove(null));
    assertThrows(IllegalArgumentException.class, () -> container.remove(otheroperatorXFX));

    assertTrue(container.remove(operatorXFX));
    assertEquals(1, container.size());
    assertNull(container.getOperatorForSimilarType(OperatorType.XFX));

    assertTrue(container.remove(operatorYF));
    assertEquals(0, container.size());
    assertNull(container.getOperatorForSimilarType(OperatorType.YF));
  }

  @Test
  public void testSize() {
    final Operator operatorFX = Operator.makeOperator(100, OperatorType.FX, "<>");
    final Operator operatorXFX = Operator.makeOperator(400, OperatorType.XFX, "<>");
    final Operator operatorYF = Operator.makeOperator(300, OperatorType.YF, "<>");

    final OperatorContainer container = new OperatorContainer(operatorFX);

    assertEquals(1, container.size());
    container.removeAll();
    assertEquals(0, container.size());
    container.addOperator(operatorFX);
    assertEquals(1, container.size());
    container.addOperator(operatorXFX);
    assertEquals(2, container.size());
    container.addOperator(operatorYF);
    assertEquals(3, container.size());
  }

  @Test
  public void testGetOperatorIfSingle() {
    final Operator operatorFX = Operator.makeOperator(100, OperatorType.FX, "<>");
    final Operator operatorXFX = Operator.makeOperator(400, OperatorType.XFX, "<>");
    final Operator operatorYF = Operator.makeOperator(300, OperatorType.YF, "<>");

    final OperatorContainer container = new OperatorContainer(operatorFX);

    assertSame(operatorFX, container.getOperatorIfSingle());
    container.addOperator(operatorYF);
    assertNull(container.getOperatorIfSingle());
    container.addOperator(operatorXFX);
    assertNull(container.getOperatorIfSingle());
    container.removeAll();
    assertNull(container.getOperatorIfSingle());

    container.addOperator(operatorYF);
    assertEquals(operatorYF, container.getOperatorIfSingle());
  }

  @Test
  public void testFindCompatibleOperator() {
    final Operator operatorFX = Operator.makeOperator(100, OperatorType.FX, "<>");
    final Operator operatorXFX = Operator.makeOperator(400, OperatorType.XFX, "<>");
    final Operator operatorYF = Operator.makeOperator(300, OperatorType.YF, "<>");

    final OperatorContainer container = new OperatorContainer(operatorFX);

    container.addOperator(operatorXFX);
    container.addOperator(operatorYF);

    assertSame(operatorFX, container.findCompatibleOperator(false, true));
    assertSame(operatorXFX, container.findCompatibleOperator(true, true));
    assertSame(operatorYF, container.findCompatibleOperator(true, false));
    assertNull(container.findCompatibleOperator(false, false));

    assertTrue(container.remove(operatorXFX));

    assertSame(operatorFX, container.findCompatibleOperator(false, true));
    assertSame(operatorFX, container.findCompatibleOperator(true, true));
    assertSame(operatorYF, container.findCompatibleOperator(true, false));
    assertNull(container.findCompatibleOperator(false, false));

    assertTrue(container.remove(operatorFX));

    assertSame(operatorYF, container.findCompatibleOperator(false, true));
    assertSame(operatorYF, container.findCompatibleOperator(true, true));
    assertSame(operatorYF, container.findCompatibleOperator(true, false));
    assertNull(container.findCompatibleOperator(false, false));

    assertTrue(container.remove(operatorYF));

    assertNull(container.findCompatibleOperator(false, true));
    assertNull(container.findCompatibleOperator(true, true));
    assertNull(container.findCompatibleOperator(true, false));
    assertNull(container.findCompatibleOperator(false, false));
  }

  @Test
  public void testGetOperatorForType() {
    final Operator operatorFX = Operator.makeOperator(100, OperatorType.FX, "<>");
    final Operator operatorXFX = Operator.makeOperator(400, OperatorType.XFX, "<>");
    final Operator operatorYF = Operator.makeOperator(300, OperatorType.YF, "<>");

    final OperatorContainer container = new OperatorContainer(operatorFX);

    assertThrows(NullPointerException.class, () -> container.getOperatorForType(null));

    assertNull(container.getOperatorForType(OperatorType.XFX));
    assertNull(container.getOperatorForType(OperatorType.YF));

    container.addOperator(operatorXFX);
    container.addOperator(operatorYF);

    assertSame(operatorFX, container.getOperatorForType(OperatorType.FX));
    assertSame(operatorXFX, container.getOperatorForType(OperatorType.XFX));
    assertSame(operatorYF, container.getOperatorForType(OperatorType.YF));
    assertNull(container.getOperatorForType(OperatorType.YFX));
    assertNull(container.getOperatorForType(OperatorType.XF));
  }

  @Test
  public void testGetOperatorForSimilarType() {
    final Operator operatorFX = Operator.makeOperator(100, OperatorType.FX, "<>");
    final Operator operatorXFX = Operator.makeOperator(400, OperatorType.XFX, "<>");
    final Operator operatorYF = Operator.makeOperator(300, OperatorType.YF, "<>");

    final OperatorContainer container = new OperatorContainer(operatorFX);
    container.addOperator(operatorXFX);
    container.addOperator(operatorYF);

    assertThrows(NullPointerException.class, () -> container.getOperatorForSimilarType(null));

    assertSame(operatorFX,
        container.getOperatorForSimilarType(OperatorType.FY));
    assertSame(operatorXFX,
        container.getOperatorForSimilarType(OperatorType.YFX));
    assertTrue(container.removeOperatorForType(OperatorType.FX));
    assertNull(container.getOperatorForSimilarType(OperatorType.FY));

  }

  @Test
  public void testRemoveOperatorForType() {
    final Operator operatorFX = Operator.makeOperator(100, OperatorType.FX, "<>");
    final Operator operatorXFX = Operator.makeOperator(400, OperatorType.XFX, "<>");
    final Operator operatorYF = Operator.makeOperator(300, OperatorType.YF, "<>");

    final OperatorContainer container = new OperatorContainer(operatorFX);
    container.addOperator(operatorXFX);
    container.addOperator(operatorYF);

    assertThrows(NullPointerException.class, () -> container.removeOperatorForType(null));

    assertTrue(container.removeOperatorForType(OperatorType.FX));
    assertNull(container.getOperatorForType(OperatorType.FX));
    assertTrue(container.removeOperatorForType(OperatorType.XFX));
    assertNull(container.getOperatorForType(OperatorType.XFX));
    assertTrue(container.removeOperatorForType(OperatorType.YF));
    assertNull(container.getOperatorForType(OperatorType.YF));
    assertFalse(container.removeOperatorForType(OperatorType.YFX));
  }

  @Test
  public void testGetText() {
    assertEquals("<>", new OperatorContainer(Operator.makeOperator(1000,
        OperatorType.FX, "<>")).getText());
  }
}
