package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class PrologStructureTest {

  @Test
  public void testGetPriority() {
    assertEquals(new PrologStructure("hello").getPrecedence(), 0);
    assertEquals(0,
        new PrologStructure(new PrologAtom("hello")).getPrecedence());

    final Operator testOperator = Operator.makeOp(666, OpType.FX, ":::");

    assertEquals(666,
        new PrologStructure(testOperator,
            new AbstractPrologTerm[] {new PrologAtom("test")}).getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals("\'Hello World\'()", new PrologStructure(new PrologAtom(
        "Hello World")).toString());
    assertEquals("!", new PrologStructure(new PrologAtom("!")).toString());

    assertEquals("'test'('', '', '')", new PrologStructure(new PrologAtom(
        "test"), 3).toString());
    assertEquals("'123'('first', 123.0, [], _)", new PrologStructure(
        new PrologAtom("123"), new AbstractPrologTerm[] {
        new PrologAtom("first"), new PrologFloatNumber(123d),
        new PrologList(), new PrologVariable()}).toString());

    final Map<String, OperatorContainer> systemOperators = GenericPrologParser.findAllSystemOperators();
    assertEquals("'hello' :- 'world'", new PrologStructure(systemOperators.get(":-").findForType(OpType.XFX),
        new AbstractPrologTerm[] {new PrologAtom("hello"),
            new PrologAtom("world")}).toString());
    assertEquals(":- 'hello'",
        new PrologStructure(systemOperators.get(":-").findForType(OpType.FX),
            new AbstractPrologTerm[] {new PrologAtom("hello")}).toString());
    assertEquals(
        "- 10 * (1 + 2)",
        new PrologStructure(
            systemOperators.get("*").findForType(
                OpType.YFX),
            new AbstractPrologTerm[] {
                new PrologStructure(
                    systemOperators.get("-").findForType(
                        OpType.FY),
                    new AbstractPrologTerm[] {new PrologIntegerNumber("10")}),
                new PrologStructure(systemOperators.get("+").findForType(OpType.YFX),
                    new AbstractPrologTerm[] {
                        new PrologIntegerNumber("1"),
                        new PrologIntegerNumber("2")})}).toString());

    assertEquals(
        "- - 10",
        new PrologStructure(
            systemOperators.get("-").findForType(
                OpType.FY),
            new AbstractPrologTerm[] {new PrologStructure(
                systemOperators.get("-").findForType(
                    OpType.FY),
                new AbstractPrologTerm[] {new PrologIntegerNumber(
                    "10")})}).toString());

    assertEquals(
        "\\ (\\+ 10)",
        new PrologStructure(
            systemOperators.get("\\").findForType(
                OpType.FY),
            new AbstractPrologTerm[] {new PrologStructure(
                systemOperators.get("\\+").findForType(
                    OpType.FY),
                new AbstractPrologTerm[] {new PrologIntegerNumber(
                    "10")})}).toString());
    assertEquals(
        "(10 .) .",
        new PrologStructure(
            systemOperators.get(".").findForType(
                OpType.XF),
            new AbstractPrologTerm[] {new PrologStructure(
                systemOperators.get(".").findForType(
                    OpType.XF),
                new AbstractPrologTerm[] {new PrologIntegerNumber(
                    "10")})}).toString());

    final Operator operatorYF = Operator.makeOp(800, OpType.YF, "!");
    final Operator operatorYF2 = Operator.makeOp(1000, OpType.YF, "!!");

    assertEquals(
        "(10 !!) !",
        new PrologStructure(
            operatorYF,
            new AbstractPrologTerm[] {new PrologStructure(
                operatorYF2,
                new AbstractPrologTerm[] {new PrologIntegerNumber(
                    "10")})}).toString());

    final Operator operatorXFX = Operator.makeOp(800, OpType.XFX, "$");
    final Operator operatorXFX2 = Operator.makeOp(1000, OpType.XFX, "$$");

    assertEquals("(10 $$ 20) $ (5 $ 30)",
        new PrologStructure(operatorXFX, new AbstractPrologTerm[] {
            new PrologStructure(operatorXFX2,
                new AbstractPrologTerm[] {
                    new PrologIntegerNumber("10"),
                    new PrologIntegerNumber("20")}),
            new PrologStructure(operatorXFX,
                new AbstractPrologTerm[] {
                    new PrologIntegerNumber("5"),
                    new PrologIntegerNumber("30")})}).toString());

    final Operator operatorXFY = Operator.makeOp(800, OpType.XFY, "$");
    final Operator operatorXFY2 = Operator.makeOp(1000, OpType.XFY, "$$");

    assertEquals("10 $ 20 $$ 5 $ 30",
        new PrologStructure(operatorXFY2, new AbstractPrologTerm[] {
            new PrologStructure(operatorXFY,
                new AbstractPrologTerm[] {
                    new PrologIntegerNumber("10"),
                    new PrologIntegerNumber("20")}),
            new PrologStructure(operatorXFY,
                new AbstractPrologTerm[] {
                    new PrologIntegerNumber("5"),
                    new PrologIntegerNumber("30")})}).toString());
  }

  @Test
  public void testGetType() {
    assertEquals(PrologTermType.STRUCT,
        new PrologStructure("hello").getType());
    assertEquals(PrologTermType.STRUCT, new PrologStructure(new PrologAtom(
        "hello")).getType());
    final Operator testOperator = Operator.makeOp(666, OpType.FX, ":::");
    assertEquals(PrologTermType.STRUCT, new PrologStructure(testOperator,
        new AbstractPrologTerm[] {new PrologAtom("test")}).getType());
  }

  @Test
  public void testPrologStructureAbstractPrologTermAbstractPrologTermArray() {
    final AbstractPrologTerm[] testterms = new AbstractPrologTerm[] {
        new PrologAtom("test1"), new PrologAtom("test2"),
        new PrologAtom("test3")};
    final AbstractPrologTerm[] testtermswithnull = new AbstractPrologTerm[] {
        new PrologAtom("test1"), null, new PrologAtom("test3")};

    assertThrows(NullPointerException.class, () -> new PrologStructure(null, testterms));
    assertThrows(NullPointerException.class, () -> new PrologStructure(new PrologAtom("hello"), null));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologFloatNumber(0.0d), testtermswithnull));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologList(), testtermswithnull));

    final PrologAtom functoratom = new PrologAtom("functor");
    PrologStructure struct = new PrologStructure(functoratom, testterms);
    assertSame(functoratom, struct.getFunctor());
    assertEquals(testterms.length, struct.getArity());

    final Operator functoroperator = Operator.makeOp(222, OpType.XFX,
        ">>>");
    struct = new PrologStructure(functoroperator, testterms);
    assertSame(functoroperator, struct.getFunctor());
    assertEquals(testterms.length, struct.getArity());

    final AbstractPrologTerm etalon = testterms[1];
    struct.setElement(1, new PrologAtom("new"));
    assertSame(etalon, testterms[1]);
  }

  @Test
  public void testPrologStructureAbstractPrologTermAbstractPrologTermArrayIntInt() {
    final AbstractPrologTerm[] testterms = new AbstractPrologTerm[] {
        new PrologAtom("test1"), new PrologAtom("test2"),
        new PrologAtom("test3")};
    final AbstractPrologTerm[] testtermswithnull = new AbstractPrologTerm[] {
        new PrologAtom("test1"), null, new PrologAtom("test3")};

    assertThrows(NullPointerException.class, () -> new PrologStructure(null, testterms, 1, 2));
    assertThrows(NullPointerException.class, () -> new PrologStructure(new PrologAtom("hello"), null, 1, 2));

    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologFloatNumber(0.0d), testtermswithnull, 1, 2));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologList(), testtermswithnull, 1, 2));

    final PrologAtom functoratom = new PrologAtom("functor");
    PrologStructure struct = new PrologStructure(functoratom, testterms, 1, 2);
    assertEquals(1, struct.getStrPosition());
    assertEquals(2, struct.getLineNumber());
  }

  @Test
  public void testPrologStructureString() {
    assertThrows(NullPointerException.class, () -> new PrologStructure((String) null));

    final PrologStructure struct = new PrologStructure("Hello World");
    assertNotNull(struct.getFunctor());
    assertEquals(PrologTermType.ATOM, struct.getFunctor().getType());
    assertEquals("Hello World", struct.getFunctor().getText());

    final PrologStructure struct2 = new PrologStructure("1111");
    assertNotNull(struct2.getFunctor());
    assertEquals(PrologTermType.ATOM, struct2.getFunctor().getType());
    assertFalse(struct2.getFunctor() instanceof AbstractPrologNumericTerm);
    assertEquals("1111", struct2.getFunctor().getText());

  }

  @Test
  public void testPrologStructureStringIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologStructure((String) null, 1, 2));

    final PrologStructure struct = new PrologStructure("Hello World", 1, 2);
    assertEquals(1, struct.getStrPosition());
    assertEquals(2, struct.getLineNumber());
  }

  @Test
  public void testPrologStructureAbstractPrologTerm() {
    final PrologAtom atom = new PrologAtom("atom1");
    final Operator operator = Operator.makeOp(6, OpType.FX, "...");

    assertThrows(NullPointerException.class, () -> new PrologStructure((AbstractPrologTerm) null));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologFloatNumber(0.0d)));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologList()));

    assertSame(atom, new PrologStructure(atom).getFunctor());
    assertSame(operator, new PrologStructure(operator).getFunctor());
  }

  @Test
  public void testPrologStructureAbstractPrologTermIntInt() {
    final PrologAtom atom = new PrologAtom("atom1");
    assertThrows(NullPointerException.class, () -> new PrologStructure((AbstractPrologTerm) null, 1, 2));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologFloatNumber(0.0d), 1, 2));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologList(), 1, 2));

    final PrologStructure test = new PrologStructure(atom, 1, 2);

    assertEquals(1, test.getStrPosition());
    assertEquals(2, test.getLineNumber());
  }

  @Test
  public void testPrologStructureAbstractPrologTermInt() {
    assertThrows(NullPointerException.class, () -> new PrologStructure(null, 4));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologAtom("test"), -1));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologVariable(), 10));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologList(), 10));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologIntegerNumber("5"), 10));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologFloatNumber(5.0d), 10));

    final PrologAtom testAtom = new PrologAtom("test");
    assertEquals(0, new PrologStructure(testAtom, 0).getArity());

    final PrologStructure struct = new PrologStructure(testAtom, 10);
    for (int index = 0; index < 10; index++) {
      assertEquals("", struct.getElement(index).getText());
    }
  }

  @Test
  public void testPrologStructureAbstractPrologTermIntIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologStructure(null, 4, 1, 2));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologAtom("test"), -1, 1, 2));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologVariable(), 10, 1, 2));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologList(), 10, 1, 2));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologIntegerNumber("5"), 10, 1, 2));
    assertThrows(IllegalArgumentException.class, () -> new PrologStructure(new PrologFloatNumber(5.0d), 10, 1, 2));

    final PrologAtom testAtom = new PrologAtom("test");

    final PrologStructure struct = new PrologStructure(testAtom, 10, 1, 2);

    assertEquals(1, struct.getStrPosition());
    assertEquals(2, struct.getLineNumber());
  }

  @Test
  public void testGetArity() {
    final AbstractPrologTerm[] testterms = new AbstractPrologTerm[] {
        new PrologAtom("test1"), new PrologAtom("test2"),
        new PrologAtom("test3")};

    assertEquals(testterms.length, new PrologStructure(new PrologAtom(
        "hello"), testterms).getArity());
    assertEquals(8,
        new PrologStructure(new PrologAtom("test"), 8).getArity());
  }

  @Test
  public void testGetElement() {
    final String[] strings = new String[] {"test1", "test2", "test3",
        "test4", "test5", "test6", "test7", "test8"};
    final AbstractPrologTerm[] terms = new AbstractPrologTerm[strings.length];
    for (int i = 0; i < strings.length; i++) {
      terms[i] = new PrologAtom(strings[i]);
    }

    final PrologStructure structure = new PrologStructure(new PrologAtom(
        "functor"), terms);
    for (int li = 0; li < strings.length; li++) {
      assertEquals(strings[li], structure.getElement(li).getText());
    }

    assertThrows(ArrayIndexOutOfBoundsException.class, () -> structure.getElement(-1));
    assertThrows(ArrayIndexOutOfBoundsException.class, () -> structure.getElement(strings.length));
  }

  @Test
  public void testSetElement() {
    final String[] strings = new String[] {"test1", "test2", "test3",
        "test4", "test5", "test6", "test7", "test8"};
    final AbstractPrologTerm[] terms = new AbstractPrologTerm[strings.length];
    for (int i = 0; i < strings.length; i++) {
      terms[i] = new PrologAtom(strings[i]);
    }

    final PrologStructure structure = new PrologStructure(new PrologAtom(
        "functor"), terms.length);
    for (int li = 0; li < terms.length; li++) {
      structure.setElement(li, terms[li]);
    }

    assertThrows(ArrayIndexOutOfBoundsException.class, () -> structure.setElement(-1, new PrologList()));
    assertThrows(ArrayIndexOutOfBoundsException.class, () -> structure.setElement(terms.length, new PrologList()));

    for (int li = 0; li < terms.length; li++) {
      assertSame(terms[li], structure.getElement(li));
    }
  }

  @Test
  public void testGetFunctor() {
    final PrologAtom functoratom = new PrologAtom("testfunctor");
    final Operator functoroperator = Operator.makeOp(666, OpType.FX,
        ":::");

    assertEquals(functoratom, new PrologStructure(functoratom).getFunctor());
    assertEquals(functoratom,
        new PrologStructure(functoratom, 3).getFunctor());
    assertEquals(new PrologStructure(functoroperator).getFunctor(),
        functoroperator);
  }

  @Test
  public void testGetText() {
    assertEquals("test", new PrologStructure("test").getText());
    assertEquals("test",
        new PrologStructure(new PrologAtom("test"), 5).getText());
    assertEquals("<<<", new PrologStructure(Operator.makeOp(222,
        OpType.FY, "<<<"), 5).getText());
  }

  @Test
  public void testCopyWithAnotherFunctor() {
    final PrologAtom functor = new PrologAtom("hello");
    final PrologAtom element = new PrologAtom("world");
    final PrologStructure struct = new PrologStructure(functor, new AbstractPrologTerm[] {element}, 10, 20);

    final PrologAtom newFunctor = new PrologAtom("haha", 3, 4);
    final PrologStructure copy = struct.copyWithAnotherFunctor(newFunctor);

    assertEquals(newFunctor, copy.getFunctor());
    assertEquals(1, copy.getArity());
    assertEquals(element, copy.getElement(0));
    assertEquals(-1, copy.getStrPosition());
    assertEquals(-1, copy.getLineNumber());
  }
}
