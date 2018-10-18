package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.EdinburghPrologParser;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Op;
import com.igormaznitsa.prologparser.operators.OpContainer;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloat;
import com.igormaznitsa.prologparser.terms.PrologInteger;
import com.igormaznitsa.prologparser.terms.PrologNumeric;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.io.StringReader;

import static com.igormaznitsa.prologparser.operators.OpContainer.make;
import static com.igormaznitsa.prologparser.terms.PrologTerm.QuotingType.NO_QUOTED;
import static com.igormaznitsa.prologparser.terms.PrologTerm.QuotingType.SINGLE_QUOTED;
import static com.igormaznitsa.prologparser.tokenizer.TokenizerState.ATOM;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class TokenizerTest {

  private Tokenizer tokenizeOf(final String str) {
    return this.tokenizeOf(str, mock(ParserContext.class));
  }

  private Tokenizer tokenizeOf(final String str, final boolean allowBlockComment) {
    ParserContext context = mock(ParserContext.class);
    when(context.getFlags()).thenReturn(ParserContext.FLAG_BLOCK_COMMENTS);
    return this.tokenizeOf(str, context);
  }

  private Tokenizer tokenizeOf(final String str, final ParserContext context) {
    return new Tokenizer(new EdinburghPrologParser(new StringReader(str), context), new StringReader(str));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testPushTermBack() {
    Tokenizer tokenizer = tokenizeOf("");
    assertNull(tokenizer.getLastPushed());
    final TokenizerResult tokenizerResult = new TokenizerResult(mock(SoftObjectPool.class)).setData(new PrologAtom("test"), ATOM, 2, 1);
    tokenizer.push(tokenizerResult);
    assertSame(tokenizerResult, tokenizer.readNextToken());
  }

  @Test
  public void testBlockComment_TermBetweenComments() {
    Tokenizer tokenizer = tokenizeOf("/* some text */hryam/*other*/.", true);
    TokenizerResult result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, result.getResult().getTermType());
    assertEquals("hryam", result.getResult().getTermText());
    assertEquals(1, result.getLine());
    assertEquals(16, result.getPos());

    tokenizer = tokenizeOf("/* some text */ 12345/*other*/.", true);
    result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, result.getResult().getTermType());
    assertEquals(12345, ((PrologNumeric) result.getResult()).getNumber().intValue());
    assertEquals(1, result.getLine());
    assertEquals(17, result.getPos());

    tokenizer = tokenizeOf("/* some text */12.345/*other*/.", true);
    result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, result.getResult().getTermType());
    assertEquals(12.345d, ((PrologNumeric) result.getResult()).getNumber().doubleValue(), Double.MIN_NORMAL);
    assertEquals(1, result.getLine());
    assertEquals(16, result.getPos());

    tokenizer = tokenizeOf("/* some text */[]/*other*/.", true);
    result = tokenizer.readNextToken();
    assertEquals(TermType.__OPERATOR_CONTAINER__, result.getResult().getTermType());
    assertEquals("[", result.getResult().getTermText());
    assertEquals(1, result.getLine());
    assertEquals(16, result.getPos());
    result = tokenizer.readNextToken();
    assertEquals(TermType.__OPERATOR_CONTAINER__, result.getResult().getTermType());
    assertEquals("]", result.getResult().getTermText());
    assertEquals(1, result.getLine());
    assertEquals(17, result.getPos());
    result = tokenizer.readNextToken();
    assertEquals(TermType.__OPERATOR_CONTAINER__, result.getResult().getTermType());
    assertEquals(".", result.getResult().getTermText());
    assertEquals(1, result.getLine());
    assertEquals(27, result.getPos());
  }

  @Test
  public void testPeekToken() {
    final Tokenizer tokenizer = tokenizeOf("hello world");

    assertEquals("hello", tokenizer.peek().getResult().getTermText());
    assertEquals("hello", tokenizer.peek().getResult().getTermText());
    assertEquals("hello", tokenizer.peek().getResult().getTermText());
    assertEquals("hello", tokenizer.peek().getResult().getTermText());
    assertEquals("hello", tokenizer.peek().getResult().getTermText());
    assertEquals("hello", tokenizer.peek().getResult().getTermText());

    assertEquals("hello", tokenizer.readNextToken().getResult().getTermText());
    assertEquals("world", tokenizer.readNextToken().getResult().getTermText());

    assertNull(tokenizer.readNextToken());
  }

  @Test
  public void testGetLastTokenStr() {
    Tokenizer tokenizer = tokenizeOf("aaa%it's a comment string nd we must skip it until the next string char \n     123 \'hello\'");
    assertNotNull(tokenizer.readNextToken());
    assertEquals(1, tokenizer.getLastTokenPos());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(6, tokenizer.getLastTokenPos());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(10, tokenizer.getLastTokenPos());
  }

  @Test
  public void testGetLastTokenLine() {
    Tokenizer tokenizer = tokenizeOf("212\n%it's a comment string nd we must skip it until the next string char \n     123\n\'hello\'");
    assertNotNull(tokenizer.readNextToken());
    assertEquals(1, tokenizer.getLastTokenLine());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(3, tokenizer.getLastTokenLine());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(4, tokenizer.getLastTokenLine());
  }

  @Test
  public void testNextToken() {
    Tokenizer tokenizer = tokenizeOf("     123 222.34 \n111.2e+4 \'string\' \n:- Variable _var _ :--");

    TokenizerResult result = tokenizer.readNextToken();
    assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
    assertEquals(TermType.ATOM, result.getResult().getTermType());
    assertEquals("123", result.getResult().getTermText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.FLOAT, result.getTokenizerState());
    assertEquals(TermType.ATOM, result.getResult().getTermType());
    assertEquals("222.34", result.getResult().getTermText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.FLOAT, result.getTokenizerState());
    assertEquals(TermType.ATOM, result.getResult().getTermType());
    assertEquals("1.112E+6", result.getResult().getTermText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.STRING, result.getTokenizerState());
    assertEquals(TermType.ATOM, result.getResult().getTermType());
    assertEquals("string", result.getResult().getTermText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
    assertEquals(TermType.__OPERATOR_CONTAINER__, result.getResult().getTermType());
    assertEquals(":-", result.getResult().getTermText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.VAR, result.getTokenizerState());
    assertEquals(TermType.VAR, result.getResult().getTermType());
    assertEquals("Variable", result.getResult().getTermText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.VAR, result.getTokenizerState());
    assertEquals(TermType.VAR, result.getResult().getTermType());
    assertEquals("_var", result.getResult().getTermText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.VAR, result.getTokenizerState());
    assertEquals(TermType.VAR, result.getResult().getTermType());
    assertEquals("_", result.getResult().getTermText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
    assertEquals(TermType.__OPERATOR_CONTAINER__, result.getResult().getTermType());
    assertEquals(":-", result.getResult().getTermText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
    assertEquals(TermType.__OPERATOR_CONTAINER__, result.getResult().getTermType());
    assertEquals("-", result.getResult().getTermText());

    assertNull(tokenizer.readNextToken());

    tokenizer = tokenizeOf(Long.toString(Long.MIN_VALUE + 1) + ' ' + Long.toString(Long.MAX_VALUE));

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, result.getTokenizerState());
    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
    assertEquals(Long.MAX_VALUE, ((PrologInteger) result.getResult()).getNumber().longValue());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
    assertEquals(Long.MAX_VALUE, ((PrologInteger) result.getResult()).getNumber().longValue(), "Negative intger will be splitted to two parts - minus and positive number part");
  }

  private void assertParserExceptionAt(final int line, final int pos, final Executable executable) {
    final PrologParserException ex = assertThrows(PrologParserException.class, executable);
    assertEquals(line, ex.getLine(), "Wrong line");
    assertEquals(pos, ex.getPos(), "Wrong pos");
  }

  @Test
  public void testUnderscoreInNumbers_Error() {
    assertParserExceptionAt(1, 4, () -> tokenizeOf("12__34.").readNextToken());
    assertParserExceptionAt(1, 7, () -> tokenizeOf("12_34_.").readNextToken());
    assertParserExceptionAt(1, 7, () -> tokenizeOf("12_34__.").readNextToken());
    assertParserExceptionAt(1, 9, () -> tokenizeOf("12_34.3__4.").readNextToken());
    assertParserExceptionAt(1, 11, () -> tokenizeOf("12_34.3_4_.").readNextToken());
    assertParserExceptionAt(1, 7, () -> tokenizeOf("12_34._34.").readNextToken());
    assertParserExceptionAt(1, 7, () -> tokenizeOf("12_34_.34.").readNextToken());
    assertParserExceptionAt(1, 10, () -> tokenizeOf("12_34.34_e+10.").readNextToken());
    assertParserExceptionAt(1, 14, () -> tokenizeOf("12_34.34e+10_.").readNextToken());
  }

  @Test
  public void testQuoting() {
    assertEquals(PrologTerm.QuotingType.NO_QUOTED, tokenizeOf("abc.").readNextToken().getResult().getQuotingType());
    assertEquals(PrologTerm.QuotingType.SINGLE_QUOTED, tokenizeOf("'abc'.").readNextToken().getResult().getQuotingType());
    assertEquals(PrologTerm.QuotingType.DOUBLE_QUOTED, tokenizeOf("\"abc\".").readNextToken().getResult().getQuotingType());
    assertEquals(PrologTerm.QuotingType.BACK_QUOTED, tokenizeOf("`abc`.").readNextToken().getResult().getQuotingType());
  }

  @Test
  public void testUnderscoreInNumbers_Normal() {
    assertEquals(12345, ((PrologInteger) tokenizeOf("12_345.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(12345, ((PrologInteger) tokenizeOf("12_34_5.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(12345, ((PrologInteger) tokenizeOf("1_2_34_5.").readNextToken().getResult()).getNumber().intValue());

    assertEquals(123.45f, ((PrologFloat) tokenizeOf("12_3.45.").readNextToken().getResult()).getNumber().floatValue(), Float.MIN_NORMAL);
    assertEquals(123.45f, ((PrologFloat) tokenizeOf("12_3.4_5.").readNextToken().getResult()).getNumber().floatValue(), Float.MIN_NORMAL);
    assertEquals(123.45f, ((PrologFloat) tokenizeOf("1_2_3.4_5.").readNextToken().getResult()).getNumber().floatValue(), Float.MIN_NORMAL);
    assertEquals(123.45e+10f, ((PrologFloat) tokenizeOf("1_2_3.4_5e+10.").readNextToken().getResult()).getNumber().floatValue(), Float.MIN_NORMAL);
    assertEquals(123.45e-10f, ((PrologFloat) tokenizeOf("1_2_3.4_5e-1_0.").readNextToken().getResult()).getNumber().floatValue(), Float.MIN_NORMAL);
    assertEquals(123.45e-10f, ((PrologFloat) tokenizeOf("1_2_3.4_5E-1_0.").readNextToken().getResult()).getNumber().floatValue(), Float.MIN_NORMAL);
  }

  @Test
  public void testMakeTermFromString() {
    final Tokenizer tokenizer = tokenizeOf("792394382");
    PrologTerm term = tokenizer.makeTermFromString("792394382", NO_QUOTED, TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologInteger.class);
    assertEquals("792394382", term.getTermText());

    term = tokenizer.makeTermFromString(Long.toString(Long.MIN_VALUE), NO_QUOTED, TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologInteger.class);
    assertEquals(Long.toString(Long.MIN_VALUE), term.getTermText());

    term = tokenizer.makeTermFromString(Long.toString(Long.MAX_VALUE), NO_QUOTED, TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologInteger.class);
    assertEquals(Long.toString(Long.MAX_VALUE), term.getTermText());

    term = tokenizer.makeTermFromString("0.003422", NO_QUOTED, TokenizerState.FLOAT);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologFloat.class);
    assertEquals("0.003422", term.getTermText());

    term = tokenizer.makeTermFromString("a0.003422b", NO_QUOTED, TokenizerState.FLOAT);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("a0.003422b", term.getTermText());

    term = tokenizer.makeTermFromString("a12345b", NO_QUOTED, TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("a12345b", term.getTermText());

    term = tokenizer.makeTermFromString("123", SINGLE_QUOTED, ATOM);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("123", term.getTermText());

    term = tokenizer.makeTermFromString("123.123", SINGLE_QUOTED, ATOM);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("123.123", term.getTermText());

    term = tokenizer.makeTermFromString("abcd", NO_QUOTED, ATOM);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("abcd", term.getTermText());
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

    final OpContainer operatorContainer = make(Op.makeOne(1000, OpType.FX, "some_operator"));

    when(context.findOperatorForName(any(GenericPrologParser.class), eq("some_operator"))).thenReturn(operatorContainer);

    final OpContainer systemOne = tokenizer.findOperatorForName(":-");
    assertNotNull(systemOne);
    assertEquals(":-", systemOne.getTermText());

    assertNull(tokenizer.findOperatorForName("%%%%%%%<unsupported_operator>%%%%%%"));
    assertSame(tokenizer.findOperatorForName("some_operator"), operatorContainer);
  }
}
