package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.DefaultParserContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.ParserContextChain;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.exceptions.CharBufferOverflowException;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloat;
import com.igormaznitsa.prologparser.terms.PrologInt;
import com.igormaznitsa.prologparser.terms.PrologNumeric;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.io.StringReader;

import static com.igormaznitsa.prologparser.terms.OpContainer.make;
import static com.igormaznitsa.prologparser.terms.Quotation.NONE;
import static com.igormaznitsa.prologparser.terms.Quotation.SINGLE;
import static com.igormaznitsa.prologparser.tokenizer.TokenizerState.ATOM;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class TokenizerTest {

  private Tokenizer tokenizeOf(final String str) {
    return this.tokenizeOf(str, Integer.MAX_VALUE);
  }

  private Tokenizer tokenizeOf(final String str, final int maxBufferLength) {
    final ParserContext context = mock(ParserContext.class);
    when(context.getMaxTokenizerBufferLength()).thenReturn(maxBufferLength);
    return this.tokenizeOf(str, context);
  }

  private Tokenizer tokenizeOf(final String str, final boolean allowBlockComment) {
    ParserContext context = mock(ParserContext.class);
    when(context.getMaxTokenizerBufferLength()).thenReturn(1024);
    when(context.getFlags()).thenReturn(allowBlockComment ? ParserContext.FLAG_BLOCK_COMMENTS : ParserContext.FLAG_NONE);
    return this.tokenizeOf(str, context);
  }

  private Tokenizer tokenizeOf(final String str, final ParserContext context) {
    return new Tokenizer(
        new GenericPrologParser(new StringReader(str),
            new ParserContextChain(new DefaultParserContext(ParserContext.FLAG_BLOCK_COMMENTS | ParserContext.FLAG_ZERO_SINGLE_QUOTATION_CHAR_CODE, Op.SWI), context)),
        PrologParser.findMetaOps(),
        new StringReader(str));
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
    assertEquals(Long.MAX_VALUE, ((PrologInt) result.getResult()).getNumber().longValue());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.INTEGER, result.getTokenizerState());
    assertEquals(Long.MAX_VALUE, ((PrologInt) result.getResult()).getNumber().longValue(), "Negative intger will be splitted to two parts - minus and positive number part");
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
  public void testExceptionForReachBufferLimit() {
    assertEquals("123456", assertThrows(CharBufferOverflowException.class, () -> tokenizeOf("1234567890.", 5).readNextToken()).getBufferText());
    assertEquals("1234", assertThrows(CharBufferOverflowException.class, () -> tokenizeOf("1234567890.", 3).readNextToken()).getBufferText());
  }

  @Test
  public void testQuoting() {
    assertEquals(Quotation.NONE, tokenizeOf("abc.").readNextToken().getResult().getQuotation());
    assertEquals(Quotation.SINGLE, tokenizeOf("'abc'.").readNextToken().getResult().getQuotation());
    assertEquals(Quotation.DOUBLE, tokenizeOf("\"abc\".").readNextToken().getResult().getQuotation());
    assertEquals(Quotation.BACK_TICK, tokenizeOf("`abc`.").readNextToken().getResult().getQuotation());
  }

  @Test
  public void testUnderscoreInNumbers_Normal() {
    assertEquals(6384, ((PrologInt) tokenizeOf("2'001_1000_1111_0000.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(6384, ((PrologInt) tokenizeOf("0b001_1000_1111_0000.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(255, ((PrologInt) tokenizeOf("16'F_F.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(255, ((PrologInt) tokenizeOf("0xF_F.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(255, ((PrologInt) tokenizeOf("16'f_f.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(255, ((PrologInt) tokenizeOf("0xf_f.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(511, ((PrologInt) tokenizeOf("8'77_7.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(511, ((PrologInt) tokenizeOf("0o77_7.").readNextToken().getResult()).getNumber().intValue());

    assertEquals(12345, ((PrologInt) tokenizeOf("12_345.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(12345, ((PrologInt) tokenizeOf("12_34_5.").readNextToken().getResult()).getNumber().intValue());
    assertEquals(12345, ((PrologInt) tokenizeOf("1_2_34_5.").readNextToken().getResult()).getNumber().intValue());

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
    PrologTerm term = tokenizer.makeTermFromString("792394382", 10, NONE, TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologInt.class);
    assertEquals("792394382", term.getTermText());

    term = tokenizer.makeTermFromString(Long.toString(Long.MIN_VALUE), 10, NONE, TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologInt.class);
    assertEquals(Long.toString(Long.MIN_VALUE), term.getTermText());

    term = tokenizer.makeTermFromString(Long.toString(Long.MAX_VALUE), 10, NONE, TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologInt.class);
    assertEquals(Long.toString(Long.MAX_VALUE), term.getTermText());

    term = tokenizer.makeTermFromString("0.003422", 10, NONE, TokenizerState.FLOAT);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologFloat.class);
    assertEquals("0.003422", term.getTermText());

    term = tokenizer.makeTermFromString("a0.003422b", 10, NONE, TokenizerState.FLOAT);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("a0.003422b", term.getTermText());

    term = tokenizer.makeTermFromString("a12345b", 10, NONE, TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("a12345b", term.getTermText());

    term = tokenizer.makeTermFromString("123", 10, SINGLE, ATOM);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("123", term.getTermText());

    term = tokenizer.makeTermFromString("123.123", 10, SINGLE, ATOM);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("123.123", term.getTermText());

    term = tokenizer.makeTermFromString("abcd", 10, NONE, ATOM);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("abcd", term.getTermText());
  }

  @Test
  public void testHasOperatorStartsWith() {
    final ParserContext context = mock(ParserContext.class);
    when(context.getMaxTokenizerBufferLength()).thenReturn(1024);
    final Tokenizer tokenizer = tokenizeOf("", context);
    assertFalse(tokenizer.hasOperatorStartsWith("<------------------------------------------------------->"));

    when(context.hasOpStartsWith(any(GenericPrologParser.class), eq("start_with"))).thenReturn(true);

    assertTrue(tokenizer.hasOperatorStartsWith(":"));
    assertFalse(tokenizer.hasOperatorStartsWith("sstart_with"));
    assertTrue(tokenizer.hasOperatorStartsWith("start_with"));
  }

  @Test
  public void testFindOperatorForName() {
    ParserContext context = mock(ParserContext.class);
    when(context.getMaxTokenizerBufferLength()).thenReturn(1024);
    Tokenizer tokenizer = tokenizeOf("", context);
    assertThrows(NullPointerException.class, () -> tokenizer.findOperatorForName(null));
    assertNull(tokenizer.findOperatorForName("<------------------------------------------------------->"));

    final OpContainer operatorContainer = make(Op.make(1000, OpAssoc.FX, "some_operator"));

    when(context.findOpForName(any(GenericPrologParser.class), eq("some_operator"))).thenReturn(operatorContainer);

    final OpContainer systemOne = tokenizer.findOperatorForName(":-");
    assertNotNull(systemOne);
    assertEquals(":-", systemOne.getTermText());

    assertNull(tokenizer.findOperatorForName("%%%%%%%<unsupported_operator>%%%%%%"));
    assertSame(tokenizer.findOperatorForName("some_operator"), operatorContainer);
  }
}
