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
package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.AbstractPrologParser;
import com.igormaznitsa.prologparser.AbstractPrologParserTest;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.PrologParser;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;

public class PrologTermTest extends AbstractPrologParserTest {

    final ParserContext mock = mock(ParserContext.class);
    final AbstractPrologParser parser = new PrologParser(mock);

    @Before
    public void beforeTest() {
        reset(mock);
    }

    @Test
    public void testToString() throws Exception {

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
                new PrologAtom("test"), new AbstractPrologTerm[]{
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

        final PrologList list4 = new PrologList(new AbstractPrologTerm[]{
                    new PrologList(new AbstractPrologTerm[]{
                        new PrologAtom("First"), new PrologVariable()}),
                    new PrologList(new AbstractPrologTerm[]{
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
