package com.igormaznitsa.prologparser.tokenizer;

import static com.igormaznitsa.prologparser.terms.OpContainer.make;
import static com.igormaznitsa.prologparser.terms.Quotation.BACK_TICK;
import static com.igormaznitsa.prologparser.terms.Quotation.COMMENT_LINE;
import static com.igormaznitsa.prologparser.terms.Quotation.DOUBLE;
import static com.igormaznitsa.prologparser.terms.Quotation.NONE;
import static com.igormaznitsa.prologparser.terms.Quotation.SINGLE;
import static com.igormaznitsa.prologparser.tokenizer.TokenizerState.ATOM;
import static java.util.Objects.requireNonNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.igormaznitsa.prologparser.DefaultParserContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.ParserContextChain;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.exceptions.CharBufferOverflowException;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.InternalSpecialCompoundTerm;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloat;
import com.igormaznitsa.prologparser.terms.PrologInt;
import com.igormaznitsa.prologparser.terms.PrologNumeric;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.terms.TermType;
import java.io.StringReader;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

@SuppressWarnings({"DataFlowIssue", "SameParameterValue"})
public class TokenizerTest {

  private Tokenizer tokenizeOf(final String str) {
    return this.tokenizeOf(str, Integer.MAX_VALUE);
  }

  private Tokenizer tokenizeOf(final String str, final int maxBufferLength) {
    final ParserContext context = mock(ParserContext.class);
    when(context.getMaxTokenizerBufferLength()).thenReturn(maxBufferLength);
    return this.tokenizeOf(str, context);
  }

  private Tokenizer tokenizeOf(final String str, final boolean allowBlockComment,
                               final boolean commentsAsAtoms) {
    ParserContext context = mock(ParserContext.class);
    when(context.getMaxTokenizerBufferLength()).thenReturn(1024);
    when(context.getFlags()).thenReturn(
        (allowBlockComment ? ParserContext.FLAG_BLOCK_COMMENTS : ParserContext.FLAG_NONE)
            | (commentsAsAtoms ? ParserContext.FLAG_COMMENTS_AS_ATOMS : ParserContext.FLAG_NONE)
    );
    return this.tokenizeOf(str, context);
  }

  private Tokenizer tokenizeOf(final String str, final ParserContext context) {
    return new Tokenizer(
        new GenericPrologParser(new StringReader(str),
            new ParserContextChain(new DefaultParserContext(ParserContext.FLAG_BLOCK_COMMENTS |
                ParserContext.FLAG_ZERO_QUOTATION_CHARCODE).addOps(Op.SWI), context)),
        PrologParser.findMetaOps(),
        new StringReader(str));
  }

  @Test
  public void testPushTermBack() {
    Tokenizer tokenizer = tokenizeOf("");
    assertNull(tokenizer.getLastPushed());
    final TokenizerResult tokenizerResult = new TokenizerResult(new PrologAtom("test"), ATOM, 2, 1);
    tokenizer.push(tokenizerResult);
    assertSame(tokenizerResult, tokenizer.readNextToken());
  }

  @Test
  public void testBlockComment_TermBetweenBlockComments() {
    Tokenizer tokenizer = tokenizeOf("/* some text */hryam/*other*/.", true, false);
    TokenizerResult result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, requireNonNull(result).getResult().getType());
    assertEquals("hryam", result.getResult().getText());
    assertEquals(1, result.getLine());
    assertEquals(16, result.getPos());

    tokenizer = tokenizeOf("/* some text */ 12345/*other*/.", true, false);
    result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, requireNonNull(result).getResult().getType());
    assertEquals(12345, ((PrologNumeric) result.getResult()).getNumber().intValue());
    assertEquals(1, result.getLine());
    assertEquals(17, result.getPos());

    tokenizer = tokenizeOf("/* some text */12.345/*other*/.", true, false);
    result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, requireNonNull(result).getResult().getType());
    assertEquals(12.345d, ((PrologNumeric) result.getResult()).getNumber().doubleValue(),
        Double.MIN_NORMAL);
    assertEquals(1, result.getLine());
    assertEquals(16, result.getPos());

    tokenizer = tokenizeOf("/* some text */[]/*other*/.", true, false);
    result = tokenizer.readNextToken();
    assertTrue(requireNonNull(result).getResult() instanceof InternalSpecialCompoundTerm);
    assertEquals("[", result.getResult().getText());
    assertEquals(1, result.getLine());
    assertEquals(16, result.getPos());
    result = tokenizer.readNextToken();
    assertTrue(requireNonNull(result).getResult() instanceof InternalSpecialCompoundTerm);
    assertEquals("]", result.getResult().getText());
    assertEquals(1, result.getLine());
    assertEquals(17, result.getPos());
    result = tokenizer.readNextToken();
    assertTrue(requireNonNull(result).getResult() instanceof InternalSpecialCompoundTerm);
    assertEquals(".", result.getResult().getText());
    assertEquals(1, result.getLine());
    assertEquals(27, result.getPos());
  }

  @Test
  public void testLineCommentAsAtoms() {
    Tokenizer tokenizer = tokenizeOf("% Hello\nhello % 333\n     % End", false, true);

    TokenizerResult result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, requireNonNull(result).getResult().getType());
    assertEquals(COMMENT_LINE, result.getResult().getQuotation());
    assertEquals(" Hello", result.getResult().getText());
    assertEquals(1, result.getLine());
    assertEquals(1, result.getPos());

    result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, requireNonNull(result).getResult().getType());
    assertEquals(NONE, result.getResult().getQuotation());
    assertEquals("hello", result.getResult().getText());
    assertEquals(2, result.getLine());
    assertEquals(1, result.getPos());

    result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, requireNonNull(result).getResult().getType());
    assertEquals(COMMENT_LINE, result.getResult().getQuotation());
    assertEquals(" 333", result.getResult().getText());
    assertEquals(2, result.getLine());
    assertEquals(7, result.getPos());

    result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, requireNonNull(result).getResult().getType());
    assertEquals(COMMENT_LINE, result.getResult().getQuotation());
    assertEquals(" End", result.getResult().getText());
    assertEquals(3, result.getLine());
    assertEquals(6, result.getPos());
  }

  @Test
  public void testBlockComment_TermBetweenBlockComments_CommentsAsAtoms() {
    Tokenizer tokenizer = tokenizeOf("/* some text */hryam/*other*/.", true, true);

    TokenizerResult result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, requireNonNull(result).getResult().getType());
    assertEquals(Quotation.COMMENT_BLOCK, result.getResult().getQuotation());
    assertEquals(" some text ", result.getResult().getText());
    assertEquals(1, result.getLine());
    assertEquals(1, result.getPos());

    result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, requireNonNull(result).getResult().getType());
    assertEquals("hryam", result.getResult().getText());
    assertEquals(1, result.getLine());
    assertEquals(16, result.getPos());

    result = tokenizer.readNextToken();
    assertEquals(TermType.ATOM, requireNonNull(result).getResult().getType());
    assertEquals(Quotation.COMMENT_BLOCK, result.getResult().getQuotation());
    assertEquals("other", result.getResult().getText());
    assertEquals(1, result.getLine());
    assertEquals(21, result.getPos());
  }

  @Test
  public void testPeekToken() {
    final Tokenizer tokenizer = tokenizeOf("hello world");

    assertEquals("hello", tokenizer.peek().getResult().getText());
    assertEquals("hello", tokenizer.peek().getResult().getText());
    assertEquals("hello", tokenizer.peek().getResult().getText());
    assertEquals("hello", tokenizer.peek().getResult().getText());
    assertEquals("hello", tokenizer.peek().getResult().getText());
    assertEquals("hello", tokenizer.peek().getResult().getText());

    assertEquals("hello", requireNonNull(tokenizer.readNextToken()).getResult().getText());
    assertEquals("world", requireNonNull(tokenizer.readNextToken()).getResult().getText());

    assertNull(tokenizer.readNextToken());
  }

  @Test
  public void testGetLastTokenStr() {
    Tokenizer tokenizer = tokenizeOf(
        "aaa%it's a comment string nd we must skip it until the next string char \n     123 'hello'");
    assertNotNull(tokenizer.readNextToken());
    assertEquals(1, tokenizer.getLastTokenPos());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(6, tokenizer.getLastTokenPos());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(10, tokenizer.getLastTokenPos());
  }

  @Test
  public void testGetLastTokenLine() {
    Tokenizer tokenizer = tokenizeOf(
        "212\n%it's a comment string nd we must skip it until the next string char \n     123\n'hello'");
    assertNotNull(tokenizer.readNextToken());
    assertEquals(1, tokenizer.getLastTokenLine());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(3, tokenizer.getLastTokenLine());
    assertNotNull(tokenizer.readNextToken());
    assertEquals(4, tokenizer.getLastTokenLine());
  }

  @Test
  public void testNextToken() {
    Tokenizer tokenizer =
        tokenizeOf("     123 222.34 \n111.2e+4 'string' \n:- Variable _var _ :--");

    TokenizerResult result = tokenizer.readNextToken();
    assertEquals(TokenizerState.INTEGER, requireNonNull(result).getTokenizerState());
    assertEquals(TermType.ATOM, result.getResult().getType());
    assertEquals("123", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.FLOAT, requireNonNull(result).getTokenizerState());
    assertEquals(TermType.ATOM, result.getResult().getType());
    assertEquals("222.34", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.FLOAT, requireNonNull(result).getTokenizerState());
    assertEquals(TermType.ATOM, result.getResult().getType());
    assertEquals("1.112E+6", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.STRING, requireNonNull(result).getTokenizerState());
    assertEquals(TermType.ATOM, result.getResult().getType());
    assertEquals("string", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, requireNonNull(result).getTokenizerState());
    assertTrue(result.getResult() instanceof InternalSpecialCompoundTerm);
    assertEquals(":-", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.VAR, requireNonNull(result).getTokenizerState());
    assertEquals(TermType.VAR, result.getResult().getType());
    assertEquals("Variable", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.VAR, requireNonNull(result).getTokenizerState());
    assertEquals(TermType.VAR, result.getResult().getType());
    assertEquals("_var", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.VAR, requireNonNull(result).getTokenizerState());
    assertEquals(TermType.VAR, result.getResult().getType());
    assertEquals("_", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, requireNonNull(result).getTokenizerState());
    assertTrue(result.getResult() instanceof InternalSpecialCompoundTerm);
    assertEquals(":-", result.getResult().getText());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, requireNonNull(result).getTokenizerState());
    assertTrue(result.getResult() instanceof InternalSpecialCompoundTerm);
    assertEquals("-", result.getResult().getText());

    assertNull(tokenizer.readNextToken());

    tokenizer = tokenizeOf(Long.toString(Long.MIN_VALUE + 1) + ' ' + Long.MAX_VALUE);

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.OPERATOR, requireNonNull(result).getTokenizerState());
    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.INTEGER, requireNonNull(result).getTokenizerState());
    assertEquals(Long.MAX_VALUE, ((PrologInt) result.getResult()).getNumber().longValue());

    result = tokenizer.readNextToken();
    assertEquals(TokenizerState.INTEGER, requireNonNull(result).getTokenizerState());
    assertEquals(Long.MAX_VALUE, ((PrologInt) result.getResult()).getNumber().longValue(),
        "Negative intger will be splitted to two parts - minus and positive number part");
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
    assertEquals("123456", assertThrows(CharBufferOverflowException.class,
        () -> tokenizeOf("1234567890.", 5).readNextToken()).getBufferText());
    assertEquals("1234", assertThrows(CharBufferOverflowException.class,
        () -> tokenizeOf("1234567890.", 3).readNextToken()).getBufferText());
  }

  private void assertQuotation(final String expectedText, final Quotation expectedQuotation,
                               final String text) {
    final Tokenizer tokenizer = this.tokenizeOf(text);
    final TokenizerResult term = tokenizer.readNextToken();
    assertNull(tokenizer.readNextToken(), text);
    assertEquals(expectedText, term.getResult().getText());
    assertEquals(expectedQuotation, term.getResult().getQuotation());
  }

  @Test
  public void testQuoting() {
    assertQuotation("abc", NONE, "abc");
    assertQuotation("abc", SINGLE, "'abc'");
    assertQuotation("a'-m-c-- |y|q\b`. P`\r - .\"LALA", DOUBLE,
        "\"a\\'-m-c-- |y|q\\b\\`. P\\`\\r - .\\\"LALA\"");
    assertQuotation("abc", BACK_TICK, "`abc`");
  }

  @Test
  public void testUnderscoreInNumbers_Normal() {
    assertEquals(6384, ((PrologInt) requireNonNull(
        tokenizeOf("2'001_1000_1111_0000.").readNextToken()).getResult()).getNumber().intValue());
    assertEquals(6384, ((PrologInt) requireNonNull(
        tokenizeOf("0b001_1000_1111_0000.").readNextToken()).getResult()).getNumber().intValue());
    assertEquals(255, ((PrologInt) requireNonNull(tokenizeOf("16'F_F.").readNextToken())
        .getResult()).getNumber().intValue());
    assertEquals(255, ((PrologInt) requireNonNull(tokenizeOf("0xF_F.").readNextToken())
        .getResult()).getNumber().intValue());
    assertEquals(255, ((PrologInt) requireNonNull(tokenizeOf("16'f_f.").readNextToken())
        .getResult()).getNumber().intValue());
    assertEquals(255, ((PrologInt) requireNonNull(tokenizeOf("0xf_f.").readNextToken())
        .getResult()).getNumber().intValue());
    assertEquals(511, ((PrologInt) requireNonNull(tokenizeOf("8'77_7.").readNextToken())
        .getResult()).getNumber().intValue());
    assertEquals(511, ((PrologInt) requireNonNull(tokenizeOf("0o77_7.").readNextToken())
        .getResult()).getNumber().intValue());

    assertEquals(12345, ((PrologInt) requireNonNull(tokenizeOf("12_345.").readNextToken())
        .getResult()).getNumber().intValue());
    assertEquals(12345, ((PrologInt) requireNonNull(tokenizeOf("12_34_5.").readNextToken())
        .getResult()).getNumber().intValue());
    assertEquals(12345, ((PrologInt) requireNonNull(tokenizeOf("1_2_34_5.").readNextToken())
        .getResult()).getNumber().intValue());

    assertEquals(123.45f, ((PrologFloat) requireNonNull(
            tokenizeOf("12_3.45.").readNextToken()).getResult()).getNumber().floatValue(),
        Float.MIN_NORMAL);
    assertEquals(123.45f, ((PrologFloat) requireNonNull(
            tokenizeOf("12_3.4_5.").readNextToken()).getResult()).getNumber().floatValue(),
        Float.MIN_NORMAL);
    assertEquals(123.45f, ((PrologFloat) requireNonNull(
            tokenizeOf("1_2_3.4_5.").readNextToken()).getResult()).getNumber().floatValue(),
        Float.MIN_NORMAL);
    assertEquals(123.45e+10f, ((PrologFloat) requireNonNull(
            tokenizeOf("1_2_3.4_5e+10.").readNextToken()).getResult()).getNumber().floatValue(),
        Float.MIN_NORMAL);
    assertEquals(123.45e-10f, ((PrologFloat) requireNonNull(
            tokenizeOf("1_2_3.4_5e-1_0.").readNextToken()).getResult()).getNumber().floatValue(),
        Float.MIN_NORMAL);
    assertEquals(123.45e-10f, ((PrologFloat) requireNonNull(
            tokenizeOf("1_2_3.4_5E-1_0.").readNextToken()).getResult()).getNumber().floatValue(),
        Float.MIN_NORMAL);
  }

  @Test
  public void testMakeTermFromString() {
    final Tokenizer tokenizer = tokenizeOf("792394382");
    PrologTerm term = tokenizer.makeTermFromString("792394382", 10, NONE, TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologInt.class);
    assertEquals("792394382", term.getText());

    term = tokenizer.makeTermFromString(Long.toString(Long.MIN_VALUE), 10, NONE,
        TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologInt.class);
    assertEquals(Long.toString(Long.MIN_VALUE), term.getText());

    term = tokenizer.makeTermFromString(Long.toString(Long.MAX_VALUE), 10, NONE,
        TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologInt.class);
    assertEquals(Long.toString(Long.MAX_VALUE), term.getText());

    term = tokenizer.makeTermFromString("0.003422", 10, NONE, TokenizerState.FLOAT);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologFloat.class);
    assertEquals("0.003422", term.getText());

    term = tokenizer.makeTermFromString("a0.003422b", 10, NONE, TokenizerState.FLOAT);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("a0.003422b", term.getText());

    term = tokenizer.makeTermFromString("a12345b", 10, NONE, TokenizerState.INTEGER);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("a12345b", term.getText());

    term = tokenizer.makeTermFromString("123", 10, SINGLE, ATOM);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("123", term.getText());

    term = tokenizer.makeTermFromString("123.123", 10, SINGLE, ATOM);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("123.123", term.getText());

    term = tokenizer.makeTermFromString("abcd", 10, NONE, ATOM);
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getType());
    assertSame(term.getClass(), PrologAtom.class);
    assertEquals("abcd", term.getText());
  }

  @Test
  public void testHasOperatorStartsWith() {
    final ParserContext context = mock(ParserContext.class);
    when(context.getMaxTokenizerBufferLength()).thenReturn(1024);
    final Tokenizer tokenizer = tokenizeOf("", context);
    assertFalse(tokenizer.hasOperatorStartsWith(
        "<------------------------------------------------------->"));

    when(context.hasOpStartsWith(any(GenericPrologParser.class), eq("start_with"))).thenReturn(
        true);

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
    assertNull(
        tokenizer.findOperatorForName("<------------------------------------------------------->"));

    final OpContainer operatorContainer = make(Op.make(1000, OpAssoc.FX, "some_operator"));

    when(context.findOpForName(any(GenericPrologParser.class), eq("some_operator"))).thenReturn(
        operatorContainer);

    final OpContainer systemOne = tokenizer.findOperatorForName(":-");
    assertNotNull(systemOne);
    assertEquals(":-", systemOne.getText());

    assertNull(tokenizer.findOperatorForName("%%%%%%%<unsupported_operator>%%%%%%"));
    assertSame(tokenizer.findOperatorForName("some_operator"), operatorContainer);
  }
}
