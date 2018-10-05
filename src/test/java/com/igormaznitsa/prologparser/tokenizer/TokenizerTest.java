package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.EdinburghPrologParser;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloatNumber;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import org.junit.jupiter.api.Test;

import java.io.StringReader;

import static com.igormaznitsa.prologparser.operators.OperatorContainer.newOpCont;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class TokenizerTest {

  private Tokenizer tokenizeOf(final String str) {
    return this.tokenizeOf(str, mock(ParserContext.class));
  }

  private Tokenizer tokenizeOf(final String str, final ParserContext context) {
    return new Tokenizer(new EdinburghPrologParser(new StringReader(str), context), new StringReader(str));
  }

  @Test
  public void testPushTermBack() {
    Tokenizer tokenizer = tokenizeOf("");
    assertNull(tokenizer.lastPushedTerm);
    final TokenizerResult tokenizerResult = new TokenizerResult(new PrologAtom("test"), TokenizerState.ATOM, 1, 2);
    tokenizer.push(tokenizerResult);
    assertSame(tokenizerResult, tokenizer.readNextToken());
  }

  @Test
  public void testPeekToken() throws Exception {
    Tokenizer tokenizer = tokenizeOf("hello world");

    assertEquals("hello", tokenizer.peek().getResult().getText());
    assertEquals("hello", tokenizer.peek().getResult().getText());
    assertEquals("hello", tokenizer.peek().getResult().getText());
    assertEquals("hello", tokenizer.peek().getResult().getText());
    assertEquals("hello", tokenizer.peek().getResult().getText());
    assertEquals("hello", tokenizer.peek().getResult().getText());

    assertEquals("hello", tokenizer.readNextToken().getResult().getText());
    assertEquals("world", tokenizer.readNextToken().getResult().getText());

    assertNull(tokenizer.readNextToken());
  }

  @Test
  public void testGetLastTokenStrPos() throws Exception {
    Tokenizer tokenizer = tokenizeOf("aaa%it's a comment string nd we must skip it until the next string char \n     123 \'hello\'");
    assertNotNull(tokenizer.readNextToken());
    assertEquals(1, tokenizer.getLastTokenStrPos());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(6, tokenizer.getLastTokenStrPos());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(10, tokenizer.getLastTokenStrPos());
  }

  @Test
  public void testGetLastTokenLineNum() throws Exception {
    Tokenizer tokenizer = tokenizeOf("212\n%it's a comment string nd we must skip it until the next string char \n     123\n\'hello\'");
    assertNotNull(tokenizer.readNextToken());
    assertEquals(1, tokenizer.getLastTokenLineNum());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(3, tokenizer.getLastTokenLineNum());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(4, tokenizer.getLastTokenLineNum());
  }

  @Test
  public void testNextToken() throws Exception {
    Tokenizer tokenizer = tokenizeOf("     123 222.34 \n111.2e+4 \'string\' \n:- Variable _var _ :--");

    TokenizerResult result = tokenizer.readNextToken();
    assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
    assertEquals(PrologTermType.ATOM, result.getResult().getType());
    assertEquals("123", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.FLOAT, result.getTokenizerState());
    assertEquals(PrologTermType.ATOM, result.getResult().getType());
    assertEquals("222.34", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.FLOAT, result.getTokenizerState());
    assertEquals(PrologTermType.ATOM, result.getResult().getType());
    assertEquals("1.112E+6", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.STRING, result.getTokenizerState());
    assertEquals(PrologTermType.ATOM, result.getResult().getType());
    assertEquals("string", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
    assertEquals(PrologTermType.OPERATORS, result.getResult().getType());
    assertEquals(":-", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.VAR, result.getTokenizerState());
    assertEquals(PrologTermType.VAR, result.getResult().getType());
    assertEquals("Variable", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.VAR, result.getTokenizerState());
    assertEquals(PrologTermType.VAR, result.getResult().getType());
    assertEquals("_var", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.VAR, result.getTokenizerState());
    assertEquals(PrologTermType.VAR, result.getResult().getType());
    assertEquals("_", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
    assertEquals(PrologTermType.OPERATORS, result.getResult().getType());
    assertEquals(":-", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
    assertEquals(PrologTermType.OPERATORS, result.getResult().getType());
    assertEquals("-", result.getResult().getText());

    assertNull(tokenizer.readNextToken());

    tokenizer = tokenizeOf(Long.toString(Long.MIN_VALUE + 1) + ' ' + Long.toString(Long.MAX_VALUE));

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
    assertEquals(Long.MAX_VALUE, ((PrologIntegerNumber) result.getResult()).getValue().longValue());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
    assertEquals(Long.MAX_VALUE, ((PrologIntegerNumber) result.getResult()).getValue().longValue(), "Negative intger will be splitted to two parts - minus and positive number part");
  }

  @Test
  public void testMakeTermFromString() {
    final Tokenizer tokenizer = tokenizeOf("792394382");
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
    final ParserContext context = mock(ParserContext.class);
    final Tokenizer tokenizer = tokenizeOf("", context);
    assertFalse(tokenizer.hasOperatorStartsWith("<------------------------------------------------------->"));

    when(context.hasOperatorStartsWith(any(GenericPrologParser.class), eq("start_with"))).thenReturn(true);

    assertTrue(tokenizer.hasOperatorStartsWith(":"));
    assertFalse(tokenizer.hasOperatorStartsWith("sstart_with"));
    assertTrue(tokenizer.hasOperatorStartsWith("start_with"));
  }

  @Test
  public void testFindOperatorForName() {
    ParserContext context = mock(ParserContext.class);
    Tokenizer tokenizer = tokenizeOf("", context);
    assertThrows(NullPointerException.class, () -> tokenizer.findOperatorForName(null));
    assertNull(tokenizer.findOperatorForName("<------------------------------------------------------->"));

    final OperatorContainer operatorContainer = newOpCont(Operator.makeOperator(1000, OpType.FX, "some_operator"));

    when(context.findOperatorForName(any(GenericPrologParser.class), eq("some_operator"))).thenReturn(operatorContainer);

    final OperatorContainer systemOne = tokenizer.findOperatorForName(":-");
    assertNotNull(systemOne);
    assertEquals(":-", systemOne.getText());

    assertNull(tokenizer.findOperatorForName("%%%%%%%<unsupported_operator>%%%%%%"));
    assertSame(tokenizer.findOperatorForName("some_operator"), operatorContainer);
  }
}
