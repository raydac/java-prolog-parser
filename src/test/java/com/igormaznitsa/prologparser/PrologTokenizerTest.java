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
package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.operators.OperatorType;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloatNumber;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import org.junit.*;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class PrologTokenizerTest extends AbstractPrologParserTest {

    private final PrologTokenizer tokenizer = new PrologTokenizer();
    private final ParserContext mockContext = mock(ParserContext.class);
    private final AbstractPrologParser mockPrologParser = mock(AbstractPrologParser.class);

    @Before
    public void onSetUp() {
        reset(mockContext);
        when(mockPrologParser.getContext()).thenReturn(mockContext);
    }

    @Test
    public void testPushTermBack() throws Exception {
        assertNull(tokenizer.lastPushedTerm);
        final TokenizerResult tokenizerResult = new TokenizerResult(
                new PrologAtom("test"), TokenizerState.ATOM, 1, 2);
        tokenizer.pushTermBack(tokenizerResult);
        assertSame(tokenizerResult, tokenizer.nextToken(
                mock(PrologCharDataSource.class), mockPrologParser));
    }

    @Test
    public void testPeekToken() throws Exception {
        try {
            tokenizer.peekToken(null, mockPrologParser);
            fail("Must throw NPE for null reader");
        } catch (NullPointerException ex) {
        }

        final PrologCharDataSource reader = new PrologCharDataSource("hello world");

        assertEquals("hello", tokenizer.peekToken(reader, null).getResult().getText());
        assertEquals("hello", tokenizer.peekToken(reader, null).getResult().getText());
        assertEquals("hello", tokenizer.peekToken(reader, null).getResult().getText());
        assertEquals("hello", tokenizer.peekToken(reader, null).getResult().getText());
        assertEquals("hello", tokenizer.peekToken(reader, null).getResult().getText());
        assertEquals("hello", tokenizer.peekToken(reader, null).getResult().getText());

        assertEquals("hello", tokenizer.nextToken(reader, null).getResult().getText());
        assertEquals("world", tokenizer.nextToken(reader, null).getResult().getText());

        assertNull(tokenizer.nextToken(reader, null));
    }

    @Test
    public void testGetLastTokenStrPos() throws Exception {
        final PrologCharDataSource reader = new PrologCharDataSource(
                "aaa%it's a comment string nd we must skip it until the next string char \n     123 \'hello\'");
        assertNotNull(tokenizer.nextToken(reader, mockPrologParser));
        assertEquals(1, tokenizer.getLastTokenStrPos());
        assertNotNull(tokenizer.nextToken(reader, mockPrologParser));
        assertEquals(6, tokenizer.getLastTokenStrPos());
        assertNotNull(tokenizer.nextToken(reader, mockPrologParser));
        assertEquals(10, tokenizer.getLastTokenStrPos());
    }

    @Test
    public void testGetLastTokenLineNum() throws Exception {
        final PrologCharDataSource reader = new PrologCharDataSource(
                "212\n%it's a comment string nd we must skip it until the next string char \n     123\n\'hello\'");
        assertNotNull(tokenizer.nextToken(reader, mockPrologParser));
        assertEquals(1, tokenizer.getLastTokenLineNum());
        assertNotNull(tokenizer.nextToken(reader, mockPrologParser));
        assertEquals(3, tokenizer.getLastTokenLineNum());
        assertNotNull(tokenizer.nextToken(reader, mockPrologParser));
        assertEquals(4, tokenizer.getLastTokenLineNum());
    }

    @Test
    public void testFixPosition() {
        final PrologCharDataSource mockReader = mock(PrologCharDataSource.class);
        when(mockReader.getLineNumber()).thenReturn(12);
        when(mockReader.getNextCharStringPosition()).thenReturn(34);

        assertEquals(0, tokenizer.getLastTokenStrPos());
        assertEquals(0, tokenizer.getLastTokenLineNum());

        tokenizer.fixPosition(mockReader);

        assertEquals(12, tokenizer.getLastTokenLineNum());
        assertEquals(33, tokenizer.getLastTokenStrPos());
    }

    @Test
    public void testSkipUntilNextString() throws Exception {
        final PrologCharDataSource reader = new PrologCharDataSource(
                "it's a comment string so we must skip it until the next string char \n123");
        tokenizer.skipUntilNextString(reader);
        assertEquals(2, reader.getLineNumber());
        assertEquals(1, reader.getNextCharStringPosition());
        final TokenizerResult result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
        assertEquals("123", result.getResult().getText());
    }

    @Test
    public void testNextToken() throws Exception {

        try {
            tokenizer.nextToken(null, mockPrologParser);
            fail("Must throw NPE for null reader");
        } catch (NullPointerException ex) {
        }

        PrologCharDataSource reader = new PrologCharDataSource(
                "     123 222.34 \n111.2e+4 \'string\' \n:- Variable _var _ :--");

        TokenizerResult result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
        assertEquals(PrologTermType.ATOM, result.getResult().getType());
        assertEquals("123", result.getResult().getText());

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.FLOAT, result.getTokenizerState());
        assertEquals(PrologTermType.ATOM, result.getResult().getType());
        assertEquals("222.34", result.getResult().getText());

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.FLOAT, result.getTokenizerState());
        assertEquals(PrologTermType.ATOM, result.getResult().getType());
        assertEquals("1.112E+6", result.getResult().getText());

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.STRING, result.getTokenizerState());
        assertEquals(PrologTermType.ATOM, result.getResult().getType());
        assertEquals("string", result.getResult().getText());

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
        assertEquals(PrologTermType.OPERATORS, result.getResult().getType());
        assertEquals(":-", result.getResult().getText());

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.VARIABLE, result.getTokenizerState());
        assertEquals(PrologTermType.VAR, result.getResult().getType());
        assertEquals("Variable", result.getResult().getText());

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.VARIABLE, result.getTokenizerState());
        assertEquals(PrologTermType.VAR, result.getResult().getType());
        assertEquals("_var", result.getResult().getText());

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.VARIABLE, result.getTokenizerState());
        assertEquals(PrologTermType.VAR, result.getResult().getType());
        assertEquals("_", result.getResult().getText());

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
        assertEquals(PrologTermType.OPERATORS, result.getResult().getType());
        assertEquals(":-", result.getResult().getText());

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
        assertEquals(PrologTermType.OPERATORS, result.getResult().getType());
        assertEquals("-", result.getResult().getText());

        assertNull(tokenizer.nextToken(reader, mockPrologParser));

        reader = new PrologCharDataSource(Long.toString(Long.MIN_VALUE + 1) + ' ' + Long.toString(Long.MAX_VALUE));

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
        assertEquals(Long.MAX_VALUE, ((PrologIntegerNumber) result.getResult()).getValue().longValue());

        result = tokenizer.nextToken(reader, mockPrologParser);
        assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
        assertEquals("Negative intger will be splitted to two parts - minus and positive number part", Long.MAX_VALUE, ((PrologIntegerNumber) result.getResult()).getValue().longValue());
        try {
            tokenizer.nextToken(new PrologCharDataSource("    \n    \'unclosed string"), null);
        } catch (PrologParserException ex) {
            assertEquals(2, ex.getLineNumber());
            assertEquals(5, ex.getStringPosition());
        }
    }

    @Test
    public void testMakeTermFromString() {
        try {
            tokenizer.makeTermFromString(null, TokenizerState.ATOM);
            fail("Must throw NPE for null text");
        } catch (NullPointerException ex) {
        }

        try {
            tokenizer.makeTermFromString("123", null);
            fail("Must throw NPE for null state");
        } catch (NullPointerException ex) {
        }

        AbstractPrologTerm term = tokenizer.makeTermFromString("792394382", TokenizerState.INTEGER);
        assertNotNull(term);
        assertEquals(PrologTermType.ATOM, term.getType());
        assertTrue("Must be prolog integer number", term.getClass() == PrologIntegerNumber.class);
        assertEquals("792394382", term.getText());

        term = tokenizer.makeTermFromString(Long.toString(Long.MIN_VALUE), TokenizerState.INTEGER);
        assertNotNull(term);
        assertEquals(PrologTermType.ATOM, term.getType());
        assertTrue("Must be prolog integer number", term.getClass() == PrologIntegerNumber.class);
        assertEquals(Long.toString(Long.MIN_VALUE), term.getText());

        term = tokenizer.makeTermFromString(Long.toString(Long.MAX_VALUE), TokenizerState.INTEGER);
        assertNotNull(term);
        assertEquals(PrologTermType.ATOM, term.getType());
        assertTrue("Must be prolog integer number", term.getClass() == PrologIntegerNumber.class);
        assertEquals(Long.toString(Long.MAX_VALUE), term.getText());

        term = tokenizer.makeTermFromString("0.003422", TokenizerState.FLOAT);
        assertNotNull(term);
        assertEquals(PrologTermType.ATOM, term.getType());
        assertTrue("Must be prolog float number", term.getClass() == PrologFloatNumber.class);
        assertEquals("0.003422", term.getText());

        term = tokenizer.makeTermFromString("a0.003422b", TokenizerState.FLOAT);
        assertNotNull(term);
        assertEquals(PrologTermType.ATOM, term.getType());
        assertTrue("Must be atom", term.getClass() == PrologAtom.class);
        assertEquals("a0.003422b", term.getText());

        term = tokenizer.makeTermFromString("a12345b", TokenizerState.INTEGER);
        assertNotNull(term);
        assertEquals(PrologTermType.ATOM, term.getType());
        assertTrue("Must be atom", term.getClass() == PrologAtom.class);
        assertEquals("a12345b", term.getText());

        term = tokenizer.makeTermFromString("123", TokenizerState.ATOM);
        assertNotNull(term);
        assertEquals(PrologTermType.ATOM, term.getType());
        assertTrue("Must be atom", term.getClass() == PrologAtom.class);
        assertEquals("123", term.getText());

        term = tokenizer.makeTermFromString("123.123", TokenizerState.ATOM);
        assertNotNull(term);
        assertEquals(PrologTermType.ATOM, term.getType());
        assertTrue("Must be atom", term.getClass() == PrologAtom.class);
        assertEquals("123.123", term.getText());

        term = tokenizer.makeTermFromString("abcd", TokenizerState.ATOM);
        assertNotNull(term);
        assertEquals(PrologTermType.ATOM, term.getType());
        assertTrue("Must be atom", term.getClass() == PrologAtom.class);
        assertEquals("abcd", term.getText());
    }

    @Test
    public void testHasOperatorStartsWith() {
        assertFalse("Should support null as context", PrologTokenizer.hasOperatorStartsWith("<------------------------------------------------------->", null));

        when(mockContext.hasOperatorStartsWith(any(AbstractPrologParser.class), eq("start_with"))).thenReturn(true);

        assertTrue(PrologTokenizer.hasOperatorStartsWith(":", mockPrologParser));
        assertFalse(PrologTokenizer.hasOperatorStartsWith("sstart_with", mockPrologParser));
        assertTrue(PrologTokenizer.hasOperatorStartsWith("start_with", mockPrologParser));
    }

    @Test
    public void testFindOperatorForName() {
        try {
            PrologTokenizer.findOperatorForName(null, mockPrologParser);
            fail("Must throw NPE for null string");
        } catch (NullPointerException ex) {
        }

        assertNull("Should support null as context", PrologTokenizer.findOperatorForName("<------------------------------------------------------->", null));

        final OperatorContainer operatorContainer = new OperatorContainer(Operator.makeOperator(1000, OperatorType.FX, "some_operator"));

        when(mockContext.findOperatorForName(any(AbstractPrologParser.class), eq("some_operator"))).thenReturn(operatorContainer);

        final OperatorContainer systemOne = PrologTokenizer.findOperatorForName(":-", mockPrologParser);
        assertNotNull("Must be found at system operator list", systemOne);
        assertEquals(":-", systemOne.getText());

        assertNull(PrologTokenizer.findOperatorForName("%%%%%%%<unsupported_operator>%%%%%%", mockPrologParser));
        assertSame(PrologTokenizer.findOperatorForName("some_operator", mockPrologParser), operatorContainer);
    }
}
