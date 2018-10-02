package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.CharSource;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloatNumber;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class TokenizerTest {

  private final Tokenizer tokenizer = new Tokenizer();
  private final ParserContext mockContext = mock(ParserContext.class);
  private final GenericPrologParser mockPrologParser = mock(GenericPrologParser.class);

  @BeforeEach
  public void before() {
    reset(mockContext);
    when(mockPrologParser.getContext()).thenReturn(mockContext);
  }

  @Test
  public void testPushTermBack() throws Exception {
    assertNull(tokenizer.lastPushedTerm);
    final TokenizerResult tokenizerResult = new TokenizerResult(
        new PrologAtom("test"), TokenizerState.ATOM, 1, 2);
    tokenizer.push(tokenizerResult);
    assertSame(tokenizerResult, tokenizer.nextToken(
        mock(CharSource.class), mockPrologParser));
  }

  @Test
  public void testPeekToken() throws Exception {
    assertThrows(NullPointerException.class, () -> tokenizer.peek(null, mockPrologParser));

    final CharSource reader = CharSource.of("hello world");

    assertEquals("hello", tokenizer.peek(reader, null).getResult().getText());
    assertEquals("hello", tokenizer.peek(reader, null).getResult().getText());
    assertEquals("hello", tokenizer.peek(reader, null).getResult().getText());
    assertEquals("hello", tokenizer.peek(reader, null).getResult().getText());
    assertEquals("hello", tokenizer.peek(reader, null).getResult().getText());
    assertEquals("hello", tokenizer.peek(reader, null).getResult().getText());

    assertEquals("hello", tokenizer.nextToken(reader, null).getResult().getText());
    assertEquals("world", tokenizer.nextToken(reader, null).getResult().getText());

    assertNull(tokenizer.nextToken(reader, null));
  }

  @Test
  public void testGetLastTokenStrPos() throws Exception {
    final CharSource reader = CharSource.of(
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
    final CharSource reader = CharSource.of(
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
    final CharSource mockReader = mock(CharSource.class);
    when(mockReader.getLineNum()).thenReturn(12);
    when(mockReader.getStrPos()).thenReturn(34);

    assertEquals(0, tokenizer.getLastTokenStrPos());
    assertEquals(0, tokenizer.getLastTokenLineNum());

    tokenizer.fixPosition(mockReader);

    assertEquals(12, tokenizer.getLastTokenLineNum());
    assertEquals(33, tokenizer.getLastTokenStrPos());
  }

  @Test
  public void testSkipUntilNextString() throws Exception {
    final CharSource reader = CharSource.of(
        "it's a comment string so we must skip it until the next string char \n123");
    tokenizer.skipUntilNextString(reader);
    assertEquals(2, reader.getLineNum());
    assertEquals(1, reader.getStrPos());
    final TokenizerResult result = tokenizer.nextToken(reader, mockPrologParser);
    assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
    assertEquals("123", result.getResult().getText());
  }

  @Test
  public void testNextToken() throws Exception {

    assertThrows(NullPointerException.class, () -> tokenizer.nextToken(null, mockPrologParser));

    CharSource reader = CharSource.of(
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
    assertEquals(TokenizerState.VAR, result.getTokenizerState());
    assertEquals(PrologTermType.VAR, result.getResult().getType());
    assertEquals("Variable", result.getResult().getText());

    result = tokenizer.nextToken(reader, mockPrologParser);
    assertEquals(TokenizerState.VAR, result.getTokenizerState());
    assertEquals(PrologTermType.VAR, result.getResult().getType());
    assertEquals("_var", result.getResult().getText());

    result = tokenizer.nextToken(reader, mockPrologParser);
    assertEquals(TokenizerState.VAR, result.getTokenizerState());
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

    reader = CharSource.of(Long.toString(Long.MIN_VALUE + 1) + ' ' + Long.toString(Long.MAX_VALUE));

    result = tokenizer.nextToken(reader, mockPrologParser);
    assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
    result = tokenizer.nextToken(reader, mockPrologParser);
    assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
    assertEquals(Long.MAX_VALUE, ((PrologIntegerNumber) result.getResult()).getValue().longValue());

    result = tokenizer.nextToken(reader, mockPrologParser);
    assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
    assertEquals(Long.MAX_VALUE, ((PrologIntegerNumber) result.getResult()).getValue().longValue(), "Negative intger will be splitted to two parts - minus and positive number part");
    try {
      tokenizer.nextToken(CharSource.of("    \n    \'unclosed string"), null);
    } catch (PrologParserException ex) {
      assertEquals(2, ex.getLineNumber());
      assertEquals(5, ex.getStringPosition());
    }
  }

  @Test
  public void testMakeTermFromString() {
    assertThrows(NullPointerException.class, () -> tokenizer.makeTermFromString(null, TokenizerState.ATOM));
    assertThrows(NullPointerException.class, () -> tokenizer.makeTermFromString("123", null));

    AbstractPrologTerm term = tokenizer.makeTermFromString("792394382", TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(PrologTermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologIntegerNumber.class);
    assertEquals("792394382", term.getText());

    term = tokenizer.makeTermFromString(Long.toString(Long.MIN_VALUE), TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(PrologTermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologIntegerNumber.class);
    assertEquals(Long.toString(Long.MIN_VALUE), term.getText());

    term = tokenizer.makeTermFromString(Long.toString(Long.MAX_VALUE), TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(PrologTermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologIntegerNumber.class);
    assertEquals(Long.toString(Long.MAX_VALUE), term.getText());

    term = tokenizer.makeTermFromString("0.003422", TokenizerState.FLOAT);
    assertNotNull(term);
    assertEquals(PrologTermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologFloatNumber.class);
    assertEquals("0.003422", term.getText());

    term = tokenizer.makeTermFromString("a0.003422b", TokenizerState.FLOAT);
    assertNotNull(term);
    assertEquals(PrologTermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("a0.003422b", term.getText());

    term = tokenizer.makeTermFromString("a12345b", TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(PrologTermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("a12345b", term.getText());

    term = tokenizer.makeTermFromString("123", TokenizerState.ATOM);
    assertNotNull(term);
    assertEquals(PrologTermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("123", term.getText());

    term = tokenizer.makeTermFromString("123.123", TokenizerState.ATOM);
    assertNotNull(term);
    assertEquals(PrologTermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("123.123", term.getText());

    term = tokenizer.makeTermFromString("abcd", TokenizerState.ATOM);
    assertNotNull(term);
    assertEquals(PrologTermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("abcd", term.getText());
  }

  @Test
  public void testHasOperatorStartsWith() {
    assertFalse(Tokenizer.hasOperatorStartsWith("<------------------------------------------------------->", null));

    when(mockContext.hasOperatorStartsWith(any(GenericPrologParser.class), eq("start_with"))).thenReturn(true);

    assertTrue(Tokenizer.hasOperatorStartsWith(":", mockPrologParser));
    assertFalse(Tokenizer.hasOperatorStartsWith("sstart_with", mockPrologParser));
    assertTrue(Tokenizer.hasOperatorStartsWith("start_with", mockPrologParser));
  }

  @Test
  public void testFindOperatorForName() {
    assertThrows(NullPointerException.class, () -> Tokenizer.findOperatorForName(null, mockPrologParser));
    assertNull(Tokenizer.findOperatorForName("<------------------------------------------------------->", null));

    final OperatorContainer operatorContainer = new OperatorContainer(Operator.makeOperator(1000, OpType.FX, "some_operator"));

    when(mockContext.findOperatorForName(any(GenericPrologParser.class), eq("some_operator"))).thenReturn(operatorContainer);

    final OperatorContainer systemOne = Tokenizer.findOperatorForName(":-", mockPrologParser);
    assertNotNull(systemOne);
    assertEquals(":-", systemOne.getText());

    assertNull(Tokenizer.findOperatorForName("%%%%%%%<unsupported_operator>%%%%%%", mockPrologParser));
    assertSame(Tokenizer.findOperatorForName("some_operator", mockPrologParser), operatorContainer);
  }
}
