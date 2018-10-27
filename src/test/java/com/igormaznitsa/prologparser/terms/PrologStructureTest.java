package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.DefaultParserContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import org.junit.jupiter.api.Test;

import java.io.StringReader;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class PrologStructureTest {

  @Test
  public void testGetPriority() {
    assertEquals(new PrologStruct("hello").getPrecedence(), 0);
    assertEquals(0,
        new PrologStruct(new PrologAtom("hello")).getPrecedence());

    final Op testOperator = Op.make(666, OpAssoc.FX, ":::");

    assertEquals(666,
        new PrologStruct(testOperator,
            new PrologTerm[] {new PrologAtom("test")}).getPrecedence());
  }

  @Test
  public void testToString() {
    assertEquals("\'Hello World\'()", new PrologStruct(new PrologAtom(
        "Hello World")).toString());
    assertEquals("!", new PrologStruct(new PrologAtom("!")).toString());

    assertEquals("test('', '', '')", new PrologStruct(new PrologAtom(
        "test"), 3).toString());
    assertEquals("'123'(first, 123.0, [], _)", new PrologStruct(
        new PrologAtom("123"), new PrologTerm[] {
        new PrologAtom("first"), new PrologFloat(123d),
        new PrologList(), new PrologVariable()}).toString());

    final Map<String, OpContainer> contextOperators = new GenericPrologParser(new StringReader(""), new DefaultParserContext(ParserContext.TOKENIZER_FLAG_NONE, Op.SWI)).getContext().findAllOperators();
    assertEquals("hello :- world", new PrologStruct(contextOperators.get(":-").findForType(OpAssoc.XFX),
        new PrologTerm[] {new PrologAtom("hello"),
            new PrologAtom("world")}).toString());
    assertEquals(":- hello",
        new PrologStruct(contextOperators.get(":-").findForType(OpAssoc.FX),
            new PrologTerm[] {new PrologAtom("hello")}).toString());
    assertEquals(
        "- 10 * (1 + 2)",
        new PrologStruct(
            contextOperators.get("*").findForType(
                OpAssoc.YFX),
            new PrologTerm[] {
                new PrologStruct(
                    contextOperators.get("-").findForType(
                        OpAssoc.FY),
                    new PrologTerm[] {new PrologInteger("10")}),
                new PrologStruct(contextOperators.get("+").findForType(OpAssoc.YFX),
                    new PrologTerm[] {
                        new PrologInteger("1"),
                        new PrologInteger("2")})}).toString());

    assertEquals(
        "- - 10",
        new PrologStruct(
            contextOperators.get("-").findForType(
                OpAssoc.FY),
            new PrologTerm[] {new PrologStruct(
                contextOperators.get("-").findForType(
                    OpAssoc.FY),
                new PrologTerm[] {new PrologInteger(
                    "10")})}).toString());

    assertEquals(
        "\\ (\\+ 10)",
        new PrologStruct(
            contextOperators.get("\\").findForType(
                OpAssoc.FY),
            new PrologTerm[] {new PrologStruct(
                contextOperators.get("\\+").findForType(
                    OpAssoc.FY),
                new PrologTerm[] {new PrologInteger(
                    "10")})}).toString());
    final Op operatorYF = Op.make(800, OpAssoc.YF, "!");
    final Op operatorYF2 = Op.make(1000, OpAssoc.YF, "!!");

    assertEquals(
        "(10 !!) !",
        new PrologStruct(
            operatorYF,
            new PrologTerm[] {new PrologStruct(
                operatorYF2,
                new PrologTerm[] {new PrologInteger(
                    "10")})}).toString());

    final Op operatorXFX = Op.make(800, OpAssoc.XFX, "$");
    final Op operatorXFX2 = Op.make(1000, OpAssoc.XFX, "$$");

    assertEquals("(10 $$ 20) $ (5 $ 30)",
        new PrologStruct(operatorXFX, new PrologTerm[] {
            new PrologStruct(operatorXFX2,
                new PrologTerm[] {
                    new PrologInteger("10"),
                    new PrologInteger("20")}),
            new PrologStruct(operatorXFX,
                new PrologTerm[] {
                    new PrologInteger("5"),
                    new PrologInteger("30")})}).toString());

    final Op operatorXFY = Op.make(800, OpAssoc.XFY, "$");
    final Op operatorXFY2 = Op.make(1000, OpAssoc.XFY, "$$");

    assertEquals("10 $ 20 $$ 5 $ 30",
        new PrologStruct(operatorXFY2, new PrologTerm[] {
            new PrologStruct(operatorXFY,
                new PrologTerm[] {
                    new PrologInteger("10"),
                    new PrologInteger("20")}),
            new PrologStruct(operatorXFY,
                new PrologTerm[] {
                    new PrologInteger("5"),
                    new PrologInteger("30")})}).toString());
  }

  @Test
  public void testGetType() {
    assertEquals(TermType.STRUCT,
        new PrologStruct("hello").getTermType());
    assertEquals(TermType.STRUCT, new PrologStruct(new PrologAtom(
        "hello")).getTermType());
    final Op testOperator = Op.make(666, OpAssoc.FX, ":::");
    assertEquals(TermType.STRUCT, new PrologStruct(testOperator,
        new PrologTerm[] {new PrologAtom("test")}).getTermType());
  }

  @Test
  public void testPrologStructureAbstractPrologTermAbstractPrologTermArray() {
    final PrologTerm[] testterms = new PrologTerm[] {
        new PrologAtom("test1"), new PrologAtom("test2"),
        new PrologAtom("test3")};
    final PrologTerm[] testtermswithnull = new PrologTerm[] {
        new PrologAtom("test1"), null, new PrologAtom("test3")};

    assertThrows(NullPointerException.class, () -> new PrologStruct(null, testterms));
    assertThrows(NullPointerException.class, () -> new PrologStruct(new PrologAtom("hello"), null));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologFloat(0.0d), testtermswithnull));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologList(), testtermswithnull));

    final PrologAtom functoratom = new PrologAtom("functor");
    PrologStruct struct = new PrologStruct(functoratom, testterms);
    assertSame(functoratom, struct.getFunctor());
    assertEquals(testterms.length, struct.getArity());

    final Op functoroperator = Op.make(222, OpAssoc.XFX,
        ">>>");
    struct = new PrologStruct(functoroperator, testterms);
    assertSame(functoroperator, struct.getFunctor());
    assertEquals(testterms.length, struct.getArity());

    final PrologTerm etalon = testterms[1];
    struct.setElementAt(1, new PrologAtom("new"));
    assertSame(etalon, testterms[1]);
  }

  @Test
  public void testPrologStructureAbstractPrologTermAbstractPrologTermArrayIntInt() {
    final PrologTerm[] testterms = new PrologTerm[] {
        new PrologAtom("test1"), new PrologAtom("test2"),
        new PrologAtom("test3")};
    final PrologTerm[] testtermswithnull = new PrologTerm[] {
        new PrologAtom("test1"), null, new PrologAtom("test3")};

    assertThrows(NullPointerException.class, () -> new PrologStruct(null, testterms, 2, 1));
    assertThrows(NullPointerException.class, () -> new PrologStruct(new PrologAtom("hello"), null, 2, 1));

    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologFloat(0.0d), testtermswithnull, 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologList(), testtermswithnull, 2, 1));

    final PrologAtom functoratom = new PrologAtom("functor");
    PrologStruct struct = new PrologStruct(functoratom, testterms, 2, 1);
    assertEquals(1, struct.getPos());
    assertEquals(2, struct.getLine());
  }

  @Test
  public void testPrologStructureString() {
    assertThrows(NullPointerException.class, () -> new PrologStruct((String) null));

    final PrologStruct struct = new PrologStruct("Hello World");
    assertNotNull(struct.getFunctor());
    assertEquals(TermType.ATOM, struct.getFunctor().getTermType());
    assertEquals("Hello World", struct.getFunctor().getTermText());

    final PrologStruct struct2 = new PrologStruct("1111");
    assertNotNull(struct2.getFunctor());
    assertEquals(TermType.ATOM, struct2.getFunctor().getTermType());
    assertFalse(struct2.getFunctor() instanceof PrologNumeric);
    assertEquals("1111", struct2.getFunctor().getTermText());

  }

  @Test
  public void testPrologStructureStringIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologStruct((String) null, 2, 1));

    final PrologStruct struct = new PrologStruct("Hello World", 2, 1);
    assertEquals(1, struct.getPos());
    assertEquals(2, struct.getLine());
  }

  @Test
  public void testPrologStructureAbstractPrologTerm() {
    final PrologAtom atom = new PrologAtom("atom1");
    final Op operator = Op.make(6, OpAssoc.FX, "...");

    assertThrows(NullPointerException.class, () -> new PrologStruct((PrologTerm) null));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologFloat(0.0d)));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologList()));

    assertSame(atom, new PrologStruct(atom).getFunctor());
    assertSame(operator, new PrologStruct(operator).getFunctor());
  }

  @Test
  public void testPrologStructureAbstractPrologTermIntInt() {
    final PrologAtom atom = new PrologAtom("atom1");
    assertThrows(NullPointerException.class, () -> new PrologStruct((PrologTerm) null, 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologFloat(0.0d), 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologList(), 2, 1));

    final PrologStruct test = new PrologStruct(atom, 2, 1);

    assertEquals(1, test.getPos());
    assertEquals(2, test.getLine());
  }

  @Test
  public void testPrologStructureAbstractPrologTermInt() {
    assertThrows(NullPointerException.class, () -> new PrologStruct(null, 4));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologAtom("test"), -1));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologVariable(), 10));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologList(), 10));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologInteger("5"), 10));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologFloat(5.0d), 10));

    final PrologAtom testAtom = new PrologAtom("test");
    assertEquals(0, new PrologStruct(testAtom, 0).getArity());

    final PrologStruct struct = new PrologStruct(testAtom, 10);
    for (int index = 0; index < 10; index++) {
      assertEquals("", struct.getElementAt(index).getTermText());
    }
  }

  @Test
  public void testPrologStructureAbstractPrologTermIntIntInt() {
    assertThrows(NullPointerException.class, () -> new PrologStruct(null, 4, 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologAtom("test"), -1, 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologVariable(), 10, 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologList(), 10, 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologInteger("5"), 10, 2, 1));
    assertThrows(IllegalArgumentException.class, () -> new PrologStruct(new PrologFloat(5.0d), 10, 2, 1));

    final PrologAtom testAtom = new PrologAtom("test");

    final PrologStruct struct = new PrologStruct(testAtom, 10, 2, 1);

    assertEquals(1, struct.getPos());
    assertEquals(2, struct.getLine());
  }

  @Test
  public void testGetArity() {
    final PrologTerm[] testterms = new PrologTerm[] {
        new PrologAtom("test1"), new PrologAtom("test2"),
        new PrologAtom("test3")};

    assertEquals(testterms.length, new PrologStruct(new PrologAtom(
        "hello"), testterms).getArity());
    assertEquals(8,
        new PrologStruct(new PrologAtom("test"), 8).getArity());
  }

  @Test
  public void testGetElement() {
    final String[] strings = new String[] {"test1", "test2", "test3",
        "test4", "test5", "test6", "test7", "test8"};
    final PrologTerm[] terms = new PrologTerm[strings.length];
    for (int i = 0; i < strings.length; i++) {
      terms[i] = new PrologAtom(strings[i]);
    }

    final PrologStruct structure = new PrologStruct(new PrologAtom(
        "functor"), terms);
    for (int li = 0; li < strings.length; li++) {
      assertEquals(strings[li], structure.getElementAt(li).getTermText());
    }

    assertThrows(ArrayIndexOutOfBoundsException.class, () -> structure.getElementAt(-1));
    assertThrows(ArrayIndexOutOfBoundsException.class, () -> structure.getElementAt(strings.length));
  }

  @Test
  public void testSetElement() {
    final String[] strings = new String[] {"test1", "test2", "test3",
        "test4", "test5", "test6", "test7", "test8"};
    final PrologTerm[] terms = new PrologTerm[strings.length];
    for (int i = 0; i < strings.length; i++) {
      terms[i] = new PrologAtom(strings[i]);
    }

    final PrologStruct structure = new PrologStruct(new PrologAtom(
        "functor"), terms.length);
    for (int li = 0; li < terms.length; li++) {
      structure.setElementAt(li, terms[li]);
    }

    assertThrows(ArrayIndexOutOfBoundsException.class, () -> structure.setElementAt(-1, new PrologList()));
    assertThrows(ArrayIndexOutOfBoundsException.class, () -> structure.setElementAt(terms.length, new PrologList()));

    for (int li = 0; li < terms.length; li++) {
      assertSame(terms[li], structure.getElementAt(li));
    }
  }

  @Test
  public void testGetFunctor() {
    final PrologAtom functoratom = new PrologAtom("testfunctor");
    final Op functoroperator = Op.make(666, OpAssoc.FX,
        ":::");

    assertEquals(functoratom, new PrologStruct(functoratom).getFunctor());
    assertEquals(functoratom,
        new PrologStruct(functoratom, 3).getFunctor());
    assertEquals(new PrologStruct(functoroperator).getFunctor(),
        functoroperator);
  }

  @Test
  public void testGetText() {
    assertEquals("test", new PrologStruct("test").getTermText());
    assertEquals("test",
        new PrologStruct(new PrologAtom("test"), 5).getTermText());
    assertEquals("<<<", new PrologStruct(Op.make(222,
        OpAssoc.FY, "<<<"), 5).getTermText());
  }

  @Test
  public void testCopyWithAnotherFunctor() {
    final PrologAtom functor = new PrologAtom("hello");
    final PrologAtom element = new PrologAtom("world");
    final PrologStruct struct = new PrologStruct(functor, new PrologTerm[] {element}, 20, 10);

    final PrologAtom newFunctor = new PrologAtom("haha", 4, 3);
    final PrologStruct copy = struct.copyWithAnotherFunctor(newFunctor);

    assertEquals(newFunctor, copy.getFunctor());
    assertEquals(1, copy.getArity());
    assertEquals(element, copy.getElementAt(0));
    assertEquals(-1, copy.getPos());
    assertEquals(-1, copy.getLine());
  }
}
