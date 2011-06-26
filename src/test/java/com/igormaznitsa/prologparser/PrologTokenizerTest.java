package com.igormaznitsa.prologparser;

import static org.junit.Assert.*;

import org.junit.*;
import org.mockito.Mockito;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.operators.OperatorType;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloatNumber;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologTermType;

public class PrologTokenizerTest extends AbstractPrologParserTest {

	private final PrologTokenizer tokenizer = new PrologTokenizer();
	private final ParserContext mockContext = Mockito.mock(ParserContext.class);

	@Before
	public void onSetUp() {
		Mockito.reset(mockContext);
	}

	@Test
	public void testPushTermBack() throws Exception {
		assertNull(tokenizer.lastPushedTerm);
		final TokenizerResult tokenizerResult = new TokenizerResult(
				new PrologAtom("test"), TokenizerState.ATOM);
		tokenizer.pushTermBack(tokenizerResult);
		assertSame(tokenizerResult, tokenizer.nextToken(
				Mockito.mock(PrologCharDataSource.class), mockContext));
	}

	@Test
	public void testPeekToken() throws Exception {
		try {
			tokenizer.peekToken(null, mockContext);
			fail("Must throw NPE for null reader");
		}catch(NullPointerException ex){}
		
		final PrologCharDataSource reader = new PrologCharDataSource("hello world");
		
		assertEquals("hello",tokenizer.peekToken(reader, null).getResult().getText());
		assertEquals("hello",tokenizer.peekToken(reader, null).getResult().getText());
		assertEquals("hello",tokenizer.peekToken(reader, null).getResult().getText());
		assertEquals("hello",tokenizer.peekToken(reader, null).getResult().getText());
		assertEquals("hello",tokenizer.peekToken(reader, null).getResult().getText());
		assertEquals("hello",tokenizer.peekToken(reader, null).getResult().getText());

		assertEquals("hello",tokenizer.nextToken(reader, null).getResult().getText());
		assertEquals("world",tokenizer.nextToken(reader, null).getResult().getText());

		assertNull(tokenizer.nextToken(reader, null));
	}

	@Test
	public void testGetLastTokenStrPos() throws Exception {
		final PrologCharDataSource reader = new PrologCharDataSource(
				"aaa%it's a comment string nd we must skip it until the next string char \n     123 \'hello\'");
		assertNotNull(tokenizer.nextToken(reader, mockContext));
		assertEquals(1, tokenizer.getLastTokenStrPos());
		assertNotNull(tokenizer.nextToken(reader, mockContext));
		assertEquals(6, tokenizer.getLastTokenStrPos());
		assertNotNull(tokenizer.nextToken(reader, mockContext));
		assertEquals(10, tokenizer.getLastTokenStrPos());
	}

	@Test
	public void testGetLastTokenLineNum() throws Exception {
		final PrologCharDataSource reader = new PrologCharDataSource(
				"212\n%it's a comment string nd we must skip it until the next string char \n     123\n\'hello\'");
		assertNotNull(tokenizer.nextToken(reader, mockContext));
		assertEquals(1, tokenizer.getLastTokenLineNum());
		assertNotNull(tokenizer.nextToken(reader, mockContext));
		assertEquals(3, tokenizer.getLastTokenLineNum());
		assertNotNull(tokenizer.nextToken(reader, mockContext));
		assertEquals(4, tokenizer.getLastTokenLineNum());
	}

	@Test
	public void testFixPosition() {
		final PrologCharDataSource mockReader = Mockito
				.mock(PrologCharDataSource.class);
		Mockito.when(mockReader.getLineNumber()).thenReturn(12);
		Mockito.when(mockReader.getNextCharStringPosition()).thenReturn(34);

		assertEquals(0, tokenizer.getLastTokenStrPos());
		assertEquals(0, tokenizer.getLastTokenLineNum());

		tokenizer.fixPosition(mockReader);

		assertEquals(12, tokenizer.getLastTokenLineNum());
		assertEquals(33, tokenizer.getLastTokenStrPos());
	}

	@Test
		public void testSkipUntilNextString() throws Exception {
			final PrologCharDataSource reader = new PrologCharDataSource(
					"it's a comment string nd we must skip it until the next string char \n123");
			tokenizer.skipUntilNextString(reader);
			assertEquals(2, reader.getLineNumber());
			assertEquals(1, reader.getNextCharStringPosition());
			final TokenizerResult result = tokenizer.nextToken(reader, mockContext);
			assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
			assertEquals("123", result.getResult().getText());
		}

	@Test
	public void testNextToken() throws Exception {

		try {
			tokenizer.nextToken(null, mockContext);
			fail("Must throw NPE for null reader");
		} catch (NullPointerException ex) {
		}

		PrologCharDataSource reader = new PrologCharDataSource(
				"     123 222.34 \n111.2e+4 \'string\' \n:- Variable _var _ :--");

		TokenizerResult result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
		assertEquals(PrologTermType.ATOM, result.getResult().getType());
		assertEquals("123", result.getResult().getText());

		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.FLOAT, result.getTokenizerState());
		assertEquals(PrologTermType.ATOM, result.getResult().getType());
		assertEquals("222.34", result.getResult().getText());

		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.FLOAT, result.getTokenizerState());
		assertEquals(PrologTermType.ATOM, result.getResult().getType());
		assertEquals(Double.toString(111.2e+4d), result.getResult().getText());

		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.STRING, result.getTokenizerState());
		assertEquals(PrologTermType.ATOM, result.getResult().getType());
		assertEquals("string", result.getResult().getText());

		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
		assertEquals(PrologTermType.OPERATORS, result.getResult().getType());
		assertEquals(":-", result.getResult().getText());

		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.VARIABLE, result.getTokenizerState());
		assertEquals(PrologTermType.VAR, result.getResult().getType());
		assertEquals("Variable", result.getResult().getText());

		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.VARIABLE, result.getTokenizerState());
		assertEquals(PrologTermType.VAR, result.getResult().getType());
		assertEquals("_var", result.getResult().getText());

		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.VARIABLE, result.getTokenizerState());
		assertEquals(PrologTermType.VAR, result.getResult().getType());
		assertEquals("_", result.getResult().getText());

		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
		assertEquals(PrologTermType.OPERATORS, result.getResult().getType());
		assertEquals(":-", result.getResult().getText());

		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
		assertEquals(PrologTermType.OPERATORS, result.getResult().getType());
		assertEquals("-", result.getResult().getText());

		assertNull(tokenizer.nextToken(reader, mockContext));

		reader = new PrologCharDataSource(Long.toString(Long.MIN_VALUE+1)+' '+Long.toString(Long.MAX_VALUE));
		
		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
		assertEquals(Long.MAX_VALUE,((PrologIntegerNumber)result.getResult()).getValue());

		result = tokenizer.nextToken(reader, mockContext);
		assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
		assertEquals("Negative intger will be splitted to two parts - minus and positive number part",Long.MAX_VALUE,((PrologIntegerNumber)result.getResult()).getValue());
		try {
			tokenizer.nextToken(new PrologCharDataSource("    \n    \'unclosed string"), null);
		}catch(PrologParserException ex){
			assertEquals(2,ex.getLineNumber());
			assertEquals(5,ex.getStringPosition());
		}
	}

	@Test
	public void testMakeTermFromString() {
		try {
			tokenizer.makeTermFromString(null, TokenizerState.ATOM);
			fail("Must throw NPE for null text");
		}catch(NullPointerException ex){}
		
		try {
			tokenizer.makeTermFromString("123", null);
			fail("Must throw NPE for null state");
		}catch(NullPointerException ex){}

		AbstractPrologTerm term = tokenizer.makeTermFromString("792394382", TokenizerState.INTEGER);
		assertNotNull(term);
		assertEquals(PrologTermType.ATOM, term.getType());
		assertTrue("Must be prolog integer number", term.getClass() == PrologIntegerNumber.class);
		assertEquals("792394382",term.getText());

		term = tokenizer.makeTermFromString(Long.toString(Long.MIN_VALUE), TokenizerState.INTEGER);
		assertNotNull(term);
		assertEquals(PrologTermType.ATOM, term.getType());
		assertTrue("Must be prolog integer number", term.getClass() == PrologIntegerNumber.class);
		assertEquals(Long.toString(Long.MIN_VALUE),term.getText());

		term = tokenizer.makeTermFromString(Long.toString(Long.MAX_VALUE), TokenizerState.INTEGER);
		assertNotNull(term);
		assertEquals(PrologTermType.ATOM, term.getType());
		assertTrue("Must be prolog integer number", term.getClass() == PrologIntegerNumber.class);
		assertEquals(Long.toString(Long.MAX_VALUE),term.getText());

		term = tokenizer.makeTermFromString("0.003422", TokenizerState.FLOAT);
		assertNotNull(term);
		assertEquals(PrologTermType.ATOM, term.getType());
		assertTrue("Must be prolog float number", term.getClass() == PrologFloatNumber.class);
		assertEquals("0.003422",term.getText());

		term = tokenizer.makeTermFromString("a0.003422b", TokenizerState.FLOAT);
		assertNotNull(term);
		assertEquals(PrologTermType.ATOM, term.getType());
		assertTrue("Must be atom", term.getClass() == PrologAtom.class);
		assertEquals("a0.003422b",term.getText());

		term = tokenizer.makeTermFromString("a12345b", TokenizerState.INTEGER);
		assertNotNull(term);
		assertEquals(PrologTermType.ATOM, term.getType());
		assertTrue("Must be atom", term.getClass() == PrologAtom.class);
		assertEquals("a12345b",term.getText());
	
		term = tokenizer.makeTermFromString("123", TokenizerState.ATOM);
		assertNotNull(term);
		assertEquals(PrologTermType.ATOM, term.getType());
		assertTrue("Must be atom", term.getClass() == PrologAtom.class);
		assertEquals("123",term.getText());

		term = tokenizer.makeTermFromString("123.123", TokenizerState.ATOM);
		assertNotNull(term);
		assertEquals(PrologTermType.ATOM, term.getType());
		assertTrue("Must be atom", term.getClass() == PrologAtom.class);
		assertEquals("123.123",term.getText());

		term = tokenizer.makeTermFromString("abcd", TokenizerState.ATOM);
		assertNotNull(term);
		assertEquals(PrologTermType.ATOM, term.getType());
		assertTrue("Must be atom", term.getClass() == PrologAtom.class);
		assertEquals("abcd",term.getText());
	}

	@Test
	public void testHasOperatorStartsWith() {
		try {
			PrologTokenizer.hasOperatorStartsWith(null, mockContext);
			fail("Must throw NPE for null string");
		}catch(NullPointerException ex){}

		assertFalse("Should support null as context",PrologTokenizer.hasOperatorStartsWith("<------------------------------------------------------->", null));
	
		Mockito.when(mockContext.hasOperatorStartsWith("start_with")).thenReturn(true);

		assertTrue(PrologTokenizer.hasOperatorStartsWith(":",mockContext));
		assertFalse(PrologTokenizer.hasOperatorStartsWith("sstart_with",mockContext));
		assertTrue(PrologTokenizer.hasOperatorStartsWith("start_with", mockContext));
	}

	@Test
	public void testFindOperatorForName() {
		try {
			PrologTokenizer.findOperatorForName(null, mockContext);
			fail("Must throw NPE for null string");
		}catch(NullPointerException ex){}

		assertNull("Should support null as context",PrologTokenizer.findOperatorForName("<------------------------------------------------------->", null));
	
		final OperatorContainer operatorContainer = new OperatorContainer(new Operator(1000,OperatorType.FX,"some_operator"));
		
		Mockito.when(mockContext.findOperatorForName("some_operator")).thenReturn(operatorContainer);

		final OperatorContainer systemOne = PrologTokenizer.findOperatorForName(":-", mockContext);
		assertNotNull("Must be found at system operator list",systemOne);
		assertEquals(":-",systemOne.getText());
		
		assertNull(PrologTokenizer.findOperatorForName("%%%%%%%<unsupported_operator>%%%%%%", mockContext));
		assertSame(PrologTokenizer.findOperatorForName("some_operator", mockContext), operatorContainer);
	}

}
