package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.EdinburghPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.tokenizer.PrologParser;
import org.junit.jupiter.api.Test;

import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

public class PrologTermTest {

  private PrologParser parserFor(final String str) {
    return new GenericPrologParser(new StringReader(str), DefaultParserContext.of(ParserContext.FLAG_NONE, Op.SWI));
  }

  @Test
  public void testToString() {

    assertEquals("\'Hello\\nWorld\'",
        new PrologAtom("Hello\nWorld").toString());
    assertEquals("-9823742321",
        new PrologInteger("-9823742321").toString());
    assertEquals("23232", new PrologInteger("23232").toString());
    assertEquals("0", new PrologInteger("0").toString());
    assertEquals("0", new PrologInteger("0").toString());
    assertEquals("-1002.0", new PrologFloat("-1002").toString());
    assertEquals("9823423.0", new PrologFloat("9823423").toString());
    assertEquals("0.0", new PrologFloat("0").toString());
    assertEquals("0.0", new PrologFloat("-0.0").toString());

    assertEquals("X", new PrologVariable("X").toString());
    assertEquals("_", new PrologVariable().toString());

    assertEquals("test(first, 100, -1000.0)", new PrologStruct(
        new PrologAtom("test"), new PrologTerm[] {
        new PrologAtom("first"),
        new PrologInteger("100"),
        new PrologFloat(-1000d)}).toString());

    assertEquals(new PrologList().toString(), "[]");

    final PrologList list = new PrologList(new PrologAtom("head"));
    list.addAsNewListToEndOfListChain(new PrologInteger("1006")).addAsNewListToEndOfListChain(new PrologVariable("Var"));
    assertEquals("[head, 1006, Var]", list.toString());

    final PrologList list2 = new PrologList(new PrologVariable("X"));
    list2.setTail(new PrologVariable());
    assertEquals("[X|_]", list2.toString());

    final PrologList list3 = new PrologList(new PrologVariable("X"));
    list3.setTail(new PrologAtom("Test"));
    assertEquals("[X|'Test']", list3.toString());

    final PrologList list4 = new PrologList(new PrologTerm[] {
        new PrologList(new PrologTerm[] {
            new PrologAtom("First"), new PrologVariable()}),
        new PrologList(new PrologTerm[] {
            new PrologAtom("hello"), new PrologAtom("world")})});
    assertEquals("[['First', _], [hello, world]]", list4.toString());

    final PrologList list5 = new PrologList(new PrologVariable("A"));
    list5.setTail(new PrologList(new PrologVariable("B")));
    assertEquals("[A, B]", list5.toString());
  }

  @Test
  public void testOperatorToString() {

    PrologTerm term = parserFor("a+b*c.").next();
    assertEquals("a + b * c", term.toString());

    term = parserFor("X is a+b.").next();
    assertEquals("X is a + b", term.toString());

    term = parserFor("hello:-world.").next();
    assertEquals("hello :- world", term.toString());

    term = parserFor(":-hello(world).").next();
    assertEquals(":- hello(world)", term.toString());

    term = parserFor(":-hello('some world').").next();
    assertEquals(":- hello('some world')", term.toString());

    term = parserFor("[1,2,3,4,5]=[1,2,3,4|[5]].").next();
    assertEquals("[1, 2, 3, 4, 5] = [1, 2, 3, 4, 5]", term.toString());

    term = parserFor("[1,2,3,4,5]\\==[1,2,3,4|X].").next();
    assertEquals("[1, 2, 3, 4, 5] \\== [1, 2, 3, 4|X]", term.toString());
  }
}
