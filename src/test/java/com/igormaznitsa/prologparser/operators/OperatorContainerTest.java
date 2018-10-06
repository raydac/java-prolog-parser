package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.terms.PrologTermType;
import org.junit.jupiter.api.Test;

import static com.igormaznitsa.prologparser.operators.Op.makeOne;
import static com.igormaznitsa.prologparser.operators.OpContainer.newOpCont;
import static org.junit.jupiter.api.Assertions.*;

public class OperatorContainerTest {

  @Test
  public void testGetPriority() {
    assertEquals(newOpCont(makeOne(1000, OpType.FX,
        "<>")).getPrecedence(), 0);
  }

  @Test
  public void testToString() {
    final Op operator = makeOne(100, OpType.FX, "<>");
    final OpContainer container = newOpCont(operator);
    container.addOp(makeOne(300, OpType.XFX, "<>"));
    container.addOp(makeOne(800, OpType.YF, "<>"));
    assertEquals(
        "OpContainer [op(100,fx,'<>'). op(800,yf,'<>'). op(300,xfx,'<>').]",
        container.toString());
  }

  @Test
  public void testGetType() {
    assertEquals(PrologTermType.OPERATORS, newOpCont(
        makeOne(1000, OpType.FX, "<>")).getType());
  }

  @Test
  public void testOperatorContainer() {
    assertThrows(NullPointerException.class, () -> newOpCont(null));

    final Op operator = makeOne(100, OpType.FX, "<>");
    final OpContainer container = newOpCont(operator);
    assertEquals(1, container.size());
    assertSame(operator, container.getOperatorIfSingle());
  }

  @Test
  public void testAddOperator() {
    final Op operatorFX = makeOne(100, OpType.FX, "<>");
    final Op operatorXFX = makeOne(400, OpType.XFX, "<>");
    final Op operatorYF = makeOne(300, OpType.YF, "<>");
    final Op otheroperatorYF = makeOne(300, OpType.YF,
        "><");

    final OpContainer container = newOpCont(operatorFX);

    assertThrows(NullPointerException.class, () -> newOpCont(null));
    assertThrows(IllegalArgumentException.class, () -> container.addOp(otheroperatorYF));

    assertEquals(1, container.size());
    assertTrue(container.addOp(operatorXFX));
    assertEquals(2, container.size());
    assertTrue(container.addOp(operatorYF));
    assertEquals(3, container.size());

    assertFalse(container.addOp(operatorXFX));
    assertFalse(container.addOp(operatorFX));
    assertFalse(container.addOp(operatorYF));

    assertSame(operatorFX, container.findForType(OpType.FX));
    assertSame(operatorXFX, container.findForType(OpType.XFX));
    assertSame(operatorYF, container.findForType(OpType.YF));
  }

  @Test
  public void testRemoveAll() {
    final Op operatorFX = makeOne(100, OpType.FX, "<>");
    final Op operatorXFX = makeOne(400, OpType.XFX, "<>");
    final Op operatorYF = makeOne(300, OpType.YF, "<>");

    final OpContainer container = newOpCont(operatorFX);

    assertThrows(NullPointerException.class, () -> container.addOp(null));

    assertTrue(container.addOp(operatorXFX));
    assertTrue(container.addOp(operatorYF));

    assertEquals(3, container.size());
    container.removeAll();
    assertEquals(0, container.size());
    assertNull(container.findForType(OpType.FX));
    assertNull(container.findForType(OpType.XFX));
    assertNull(container.findForType(OpType.YF));
  }

  @Test
  public void testRemove() {
    final Op operatorFX = makeOne(100, OpType.FX, "<>");
    final Op operatorXFX = makeOne(400, OpType.XFX, "<>");
    final Op operatorYF = makeOne(300, OpType.YF, "<>");
    final Op otheroperatorXFX = makeOne(400, OpType.XFX,
        "><");

    final OpContainer container = newOpCont(operatorFX);
    container.addOp(operatorXFX);
    container.addOp(operatorYF);

    assertEquals(3, container.size());

    assertTrue(container.remove(operatorFX));
    assertEquals(2, container.size());
    assertNull(container.findSimilar(OpType.FX));

    assertThrows(NullPointerException.class, () -> container.remove(null));
    assertThrows(IllegalArgumentException.class, () -> container.remove(otheroperatorXFX));

    assertTrue(container.remove(operatorXFX));
    assertEquals(1, container.size());
    assertNull(container.findSimilar(OpType.XFX));

    assertTrue(container.remove(operatorYF));
    assertEquals(0, container.size());
    assertNull(container.findSimilar(OpType.YF));
  }

  @Test
  public void testSize() {
    final Op operatorFX = makeOne(100, OpType.FX, "<>");
    final Op operatorXFX = makeOne(400, OpType.XFX, "<>");
    final Op operatorYF = makeOne(300, OpType.YF, "<>");

    final OpContainer container = newOpCont(operatorFX);

    assertEquals(1, container.size());
    container.removeAll();
    assertEquals(0, container.size());
    container.addOp(operatorFX);
    assertEquals(1, container.size());
    container.addOp(operatorXFX);
    assertEquals(2, container.size());
    container.addOp(operatorYF);
    assertEquals(3, container.size());
  }

  @Test
  public void testGetOperatorIfSingle() {
    final Op operatorFX = makeOne(100, OpType.FX, "<>");
    final Op operatorXFX = makeOne(400, OpType.XFX, "<>");
    final Op operatorYF = makeOne(300, OpType.YF, "<>");

    final OpContainer container = newOpCont(operatorFX);

    assertSame(operatorFX, container.getOperatorIfSingle());
    container.addOp(operatorYF);
    assertNull(container.getOperatorIfSingle());
    container.addOp(operatorXFX);
    assertNull(container.getOperatorIfSingle());
    container.removeAll();
    assertNull(container.getOperatorIfSingle());

    container.addOp(operatorYF);
    assertEquals(operatorYF, container.getOperatorIfSingle());
  }

  @Test
  public void testFindCompatibleOperator() {
    final Op operatorFX = makeOne(100, OpType.FX, "<>");
    final Op operatorXFX = makeOne(400, OpType.XFX, "<>");
    final Op operatorYF = makeOne(300, OpType.YF, "<>");

    final OpContainer container = newOpCont(operatorFX);

    container.addOp(operatorXFX);
    container.addOp(operatorYF);

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
    final Op operatorFX = makeOne(100, OpType.FX, "<>");
    final Op operatorXFX = makeOne(400, OpType.XFX, "<>");
    final Op operatorYF = makeOne(300, OpType.YF, "<>");

    final OpContainer container = newOpCont(operatorFX);

    assertThrows(NullPointerException.class, () -> container.findForType(null));

    assertNull(container.findForType(OpType.XFX));
    assertNull(container.findForType(OpType.YF));

    container.addOp(operatorXFX);
    container.addOp(operatorYF);

    assertSame(operatorFX, container.findForType(OpType.FX));
    assertSame(operatorXFX, container.findForType(OpType.XFX));
    assertSame(operatorYF, container.findForType(OpType.YF));
    assertNull(container.findForType(OpType.YFX));
    assertNull(container.findForType(OpType.XF));
  }

  @Test
  public void testGetOperatorForSimilarType() {
    final Op operatorFX = makeOne(100, OpType.FX, "<>");
    final Op operatorXFX = makeOne(400, OpType.XFX, "<>");
    final Op operatorYF = makeOne(300, OpType.YF, "<>");

    final OpContainer container = newOpCont(operatorFX);
    container.addOp(operatorXFX);
    container.addOp(operatorYF);

    assertThrows(NullPointerException.class, () -> container.findSimilar(null));

    assertSame(operatorFX,
        container.findSimilar(OpType.FY));
    assertSame(operatorXFX,
        container.findSimilar(OpType.YFX));
    assertTrue(container.removeForType(OpType.FX));
    assertNull(container.findSimilar(OpType.FY));

  }

  @Test
  public void testRemoveOperatorForType() {
    final Op operatorFX = makeOne(100, OpType.FX, "<>");
    final Op operatorXFX = makeOne(400, OpType.XFX, "<>");
    final Op operatorYF = makeOne(300, OpType.YF, "<>");

    final OpContainer container = newOpCont(operatorFX);
    container.addOp(operatorXFX);
    container.addOp(operatorYF);

    assertThrows(NullPointerException.class, () -> container.removeForType(null));

    assertTrue(container.removeForType(OpType.FX));
    assertNull(container.findForType(OpType.FX));
    assertTrue(container.removeForType(OpType.XFX));
    assertNull(container.findForType(OpType.XFX));
    assertTrue(container.removeForType(OpType.YF));
    assertNull(container.findForType(OpType.YF));
    assertFalse(container.removeForType(OpType.YFX));
  }

  @Test
  public void testGetText() {
    assertEquals("<>", newOpCont(makeOne(1000,
        OpType.FX, "<>")).getText());
  }
}
