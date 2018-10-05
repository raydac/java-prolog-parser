package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.CharSource;
import com.igormaznitsa.prologparser.EdinburghPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;

public class PrologTermTest {

  final ParserContext mock = mock(ParserContext.class);
  final GenericPrologParser parser = new EdinburghPrologParser(mock);

  @BeforeEach
  public void beforeTest() {
    reset(mock);
  }

  @Test
  public void testToString() {

    assertEquals("\'Hello\\nWorld\'",
        new PrologAtom("Hello\nWorld").toString());
    assertEquals("-9823742321",
        new PrologIntegerNumber("-9823742321").toString());
    assertEquals("23232", new PrologIntegerNumber("23232").toString());
    assertEquals("0", new PrologIntegerNumber("0").toString());
    assertEquals("0", new PrologIntegerNumber("0").toString());
    assertEquals("-1002.0", new PrologFloatNumber("-1002").toString());
    assertEquals("9823423.0", new PrologFloatNumber("9823423").toString());
    assertEquals("0.0", new PrologFloatNumber("0").toString());
    assertEquals("0.0", new PrologFloatNumber("-0.0").toString());

    assertEquals("X", new PrologVariable("X").toString());
    assertEquals("_", new PrologVariable().toString());

    assertEquals("\'test\'(\'first\', 100, -1000.0)", new PrologStructure(
        new PrologAtom("test"), new AbstractPrologTerm[] {
        new PrologAtom("first"),
        new PrologIntegerNumber("100"),
        new PrologFloatNumber(-1000d)}).toString());

    assertEquals(new PrologList().toString(), "[]");

    final PrologList list = new PrologList(new PrologAtom("head"));
    list.addAsNewListToEndOfListChain(new PrologIntegerNumber("1006")).addAsNewListToEndOfListChain(new PrologVariable("Var"));
    assertEquals("['head', 1006, Var]", list.toString());

    final PrologList list2 = new PrologList(new PrologVariable("X"));
    list2.setTail(new PrologVariable());
    assertEquals("[X|_]", list2.toString());

    final PrologList list3 = new PrologList(new PrologVariable("X"));
    list3.setTail(new PrologAtom("Test"));
    assertEquals("[X|'Test']", list3.toString());

    final PrologList list4 = new PrologList(new AbstractPrologTerm[] {
        new PrologList(new AbstractPrologTerm[] {
            new PrologAtom("First"), new PrologVariable()}),
        new PrologList(new AbstractPrologTerm[] {
            new PrologAtom("hello"), new PrologAtom("world")})});
    assertEquals("[['First', _], ['hello', 'world']]", list4.toString());

    final PrologList list5 = new PrologList(new PrologVariable("A"));
    list5.setTail(new PrologList(new PrologVariable("B")));
    assertEquals("[A, B]", list5.toString());
  }

  @Test
  public void testOperatorToString() throws Exception {
    AbstractPrologTerm term = parser.nextSentence("a+b*c.");
    assertEquals("'a' + 'b' * 'c'", term.toString());

    term = parser.nextSentence("X is a+b.");
    assertEquals("X is 'a' + 'b'", term.toString());

    term = parser.nextSentence("hello:-world.");
    assertEquals("'hello' :- 'world'", term.toString());

    term = parser.nextSentence(":-hello(world).");
    assertEquals(":- 'hello'('world')", term.toString());

    term = parser.nextSentence("[1,2,3,4,5]=[1,2,3,4|[5]].");
    assertEquals("[1, 2, 3, 4, 5] = [1, 2, 3, 4, 5]", term.toString());

    term = parser.nextSentence("[1,2,3,4,5]\\==[1,2,3,4|X].");
    assertEquals("[1, 2, 3, 4, 5] \\== [1, 2, 3, 4|X]", term.toString());
  }
}
