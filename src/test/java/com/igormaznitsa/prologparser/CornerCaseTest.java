package com.igormaznitsa.prologparser;

import static com.igormaznitsa.prologparser.ParserContext.FLAG_BLOCK_COMMENTS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_COMMENTS_AS_ATOMS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_CURLY_BRACKETS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_ZERO_QUOTATION_CHARCODE;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.FY;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFX;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.YFX;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.prologparser.exceptions.CharBufferOverflowException;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologInt;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.TokenizerResult;
import java.io.StringReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;

class CornerCaseTest extends AbstractIntegrationTest {

  private static final int EDINBURGH_FLAGS =
      FLAG_BLOCK_COMMENTS | FLAG_ZERO_QUOTATION_CHARCODE | FLAG_CURLY_BRACKETS;

  @Test
  void testStrayClosingBracketIsSyntaxError() {
    this.assertError(").", "Unexpected closing bracket: )", 1, 1);
    this.assertError("].", "Unexpected closing bracket: ]", 1, 1);
    this.assertError("}.", "Unexpected closing bracket: }", 1, 1);
    this.assertError("a :- ).", "Unexpected closing bracket: )", 1, 6);
    this.assertError("foo(a) :- bar, ).", "Unexpected closing bracket: )", 1, 16);
    this.assertError("f(a, ]).", "Unexpected closing bracket: ]", 1, 6);
    this.assertError("f(}).", "Unexpected closing bracket: }", 1, 3);
    this.assertError("[a, )].", "Unexpected closing bracket: )", 1, 5);
    this.assertError("X = (]).", "Unexpected closing bracket: ]", 1, 6);
    this.assertError("X = [a|)].", "Unexpected closing bracket: )", 1, 8);
  }

  @Test
  void testQuotedBracketsStayAtoms() {
    assertEquals("X = ')'", this.parseSingle("X = ')'."));
    assertEquals("X = ']'", this.parseSingle("X = ']'."));
    assertEquals("X = '}'", this.parseSingle("X = '}'."));
    assertEquals("X = '('", this.parseSingle("X = '('."));
    assertEquals("f(')', ']')", this.parseSingle("f(')',']')."));
  }

  @Test
  void testClosingCurlyIsPlainAtomWithoutCurlyFlag() {
    final PrologTerm term = this.parseWithFlags("}.", FLAG_BLOCK_COMMENTS).next();
    assertEquals(TermType.ATOM, term.getType());
    assertEquals("}", term.getText());
  }

  @Test
  void testMinusBeforeNumberMakesNegativeLiteral() {
    this.assertInteger(-1, "- 1.");
    this.assertInteger(-1, "-1.");
    assertEquals("[-1]", this.parseSingle("[- 1]."));
    assertEquals("f(-1)", this.parseSingle("f(-1)."));
    assertEquals("X = - -1", this.parseSingle("X = - - 1."));
    assertEquals("X = - - 1", this.parseSingle("X = -(-(1))."));
  }

  @Test
  void testMinusAfterOperandIsInfixOperator() {
    for (final String source : List.of("X = a-1.", "X = a -1.", "X = a - 1.")) {
      final PrologTerm difference = this.rightOperand(this.parseEd(source).next());
      assertEquals("-", difference.getText(), source);
      assertEquals(2, difference.getArity(), source);
      assertEquals(BigInteger.ONE, this.intAt(difference, 1), source);
    }
    assertEquals("X = 2 - 1", this.parseSingle("X = 2 -1."));
    assertEquals("X = a - -1", this.parseSingle("X = a- -1."));
    assertEquals("X = a - (-1)", this.parseSingle("X = a-(-1)."));
  }

  @Test
  void testMinusZeroIsPlainZero() {
    this.assertInteger(0, "-0.");
  }

  @Test
  void testArbitraryPrecisionAndRadixIntegers() {
    assertEquals(new BigInteger("FFFFFFFFFFFFFFFFFFFF", 16),
        this.intAt(this.parseEd("X = 0xFFFFFFFFFFFFFFFFFFFF.").next(), 1));
    assertEquals(new BigInteger("-123456789012345678901234567890"),
        this.intAt(this.parseEd("X = -123456789012345678901234567890.").next(), 1));
    assertEquals(BigInteger.valueOf(15), this.intAt(this.parseEd("X = 0o17.").next(), 1));
    assertEquals(BigInteger.valueOf(31), this.intAt(this.parseEd("X = 0x1F.").next(), 1));
    assertEquals(BigInteger.valueOf(65), this.intAt(this.parseEd("X = 0'\\x41\\.").next(), 1));
    assertEquals(BigInteger.valueOf(92), this.intAt(this.parseEd("X = 0'\\\\.").next(), 1));
  }

  @Test
  void testMalformedRadixIntegers() {
    this.assertError("X = 0x.", "Invalid integer: ", 1, 5);
    assertThrows(PrologParserException.class, () -> this.parseEd("X = 0b102.").next());
    assertThrows(PrologParserException.class, () -> this.parseEd("X = 0' .").next());
  }

  @Test
  void testCommaIsRightAssociative() {
    final PrologTerm conjunction = this.parseEd("a, b, c.").next();
    assertEquals(",", conjunction.getText());
    assertEquals("a", this.argAt(conjunction, 0).getText());
    assertEquals(",", this.argAt(conjunction, 1).getText());
    assertEquals("c", this.argAt(this.argAt(conjunction, 1), 1).getText());
  }

  @Test
  void testYfxArithmeticIsLeftAssociative() {
    final PrologTerm difference = this.parseEd("1 - 2 - 3.").next();
    assertEquals("-", difference.getText());
    assertEquals("-", this.argAt(difference, 0).getText());
    assertEquals(BigInteger.valueOf(3), this.intAt(difference, 1));
  }

  @Test
  void testXfyPowerIsRightAssociative() {
    final PrologTerm power = this.rightOperand(this.parseEd("X = 2 ^ 3 ^ 4.").next());
    assertEquals("^", power.getText());
    assertEquals(BigInteger.valueOf(2), this.intAt(power, 0));
    assertEquals("^", this.argAt(power, 1).getText());
  }

  @Test
  void testMultiplicationBindsTighterThanAddition() {
    final PrologTerm sum = this.rightOperand(this.parseEd("X = 1 + 2 * 3.").next());
    assertEquals("+", sum.getText());
    assertEquals("*", this.argAt(sum, 1).getText());
  }

  @Test
  void testXfxOperatorsCannotChain() {
    this.assertError("X = a = b.", "Operator precedence clash or missing operator: [=]", 1, 7);
    this.assertError("X = (a=b)=c.", "Operator precedence clash or missing operator: [=]", 1, 10);
    this.assertError("X = 2 ** 3 ** 4.", "Operator precedence clash or missing operator: [**]",
        1, 12);
    this.assertError("a :- b :- c.", null, -1, -1);
  }

  @Test
  void testIfThenElseNesting() {
    final PrologTerm block = this.rightOperand(this.parseEd("X = (a -> b ; c).").next());
    final PrologTerm disjunction = this.argAt(block, 0);
    assertEquals(";", disjunction.getText());
    assertEquals("->", this.argAt(disjunction, 0).getText());
    assertEquals("c", this.argAt(disjunction, 1).getText());
  }

  @Test
  void testDirectivesQueriesAndGrammarRules() {
    assertEquals(":- dynamic foo / 1", this.parseSingle(":- dynamic foo/1."));
    assertEquals(":- dynamic foo / 1", this.parseSingle(":- dynamic(foo/1)."));
    assertEquals("?- foo(X)", this.parseSingle("?- foo(X)."));
    assertEquals("a --> b , [c]", this.parseSingle("a --> b, [c]."));
  }

  @Test
  void testOperatorAtomsAsArguments() {
    assertEquals("f('-')", this.parseSingle("f(-)."));
    assertEquals("f('-', a)", this.parseSingle("f(- , a)."));
    assertEquals("f(a, '-')", this.parseSingle("f(a, -)."));
    assertEquals("['-']", this.parseSingle("[-]."));
    assertEquals("X = - - a", this.parseSingle("X = - - a."));
    assertEquals("X = - - - a", this.parseSingle("X = - (- (- a))."));
  }

  @Test
  void testSpecialAtomsAreDistinctFromSyntax() {
    final PrologTerm emptyList = this.rightOperand(this.parseEd("X = [].").next());
    assertInstanceOf(PrologList.class, emptyList);
    assertTrue(((PrologList) emptyList).isEmpty());

    final PrologTerm quotedEmptyList = this.rightOperand(this.parseEd("X = '[]'.").next());
    assertInstanceOf(PrologAtom.class, quotedEmptyList);
    assertEquals(Quotation.SINGLE, quotedEmptyList.getQuotation());

    assertTrue(((PrologStruct) this.rightOperand(this.parseEd("X = {}.").next())).isCurlyBlock());
    assertEquals("X = '{}'(a)", this.parseSingle("X = '{}'(a)."));
    assertEquals("X = ','(a, b)", this.parseSingle("X = ','(a,b)."));
    assertEquals("X = '|'(a, b)", this.parseSingle("X = '|'(a,b)."));
    assertEquals("X = (,)", this.parseSingle("X = (,)."));
  }

  @Test
  void testListTailNormalization() {
    assertEquals("X = [a, b, c]", this.parseSingle("X = [a|[b|[c|[]]]]."));
    assertEquals("X = [[]]", this.parseSingle("X = [[]|[]]."));
    assertEquals("X = [1, 2, 3, 4|T]", this.parseSingle("X = [1,2|[3,4|T]]."));
  }

  @Test
  void testMalformedPunctuation() {
    this.assertError("X = [a|b|c].", "Duplicated list tail definition", 1, 10);
    this.assertError("X = [|].", "There is not any list element", 1, 5);
    this.assertError("X = (a,).", "No operands: [,]", 1, 7);
    this.assertError("X = ,.", "No operands: [,]", 1, 5);
    this.assertError("X = |.", "No operands: [|]", 1, 5);
    this.assertError("X = f(a, ,).", "Structure argument not found", 1, 10);
    this.assertError("f(a)(b).", "There must be an operator between atoms or structures", 1, 5);
    this.assertError("X = f(a b).", "There must be an operator between atoms or structures", 1,
        9);
    this.assertError("X = 'a' 'b'.", "There is no any operator before the atom", 1, 9);
    this.assertError("X = 1 2.", "There is no any operator before the atom", 1, 7);
  }

  @Test
  void testUnclosedOpeningBrackets() {
    this.assertError("X = (a.", "Non-ended clause", 1, 7);
    this.assertError("X = [a.", "Non-ended clause", 1, 7);
    this.assertError("X = {a.", "Non-ended clause", 1, 7);
    this.assertError("X = /*unterminated", "Unclosed block comment", 1, 19);
  }

  @Test
  void testLineCommentEndsAtNewline() {
    this.assertError("X = a%comment\nb.", "There is no any operator before the atom", 2, 1);
    assertEquals("X = a + b", this.parseSingle("X = a/*c*/+/*d*/b."));
    assertEquals("foo :- bar", this.parseSingle("foo:-/*x*/bar."));
  }

  @Test
  void testEndDotFollowedByCommentOrLayout() {
    assertEquals(List.of("a"), this.parseAllAsText("a.%c"));
    assertEquals(List.of("a"), this.parseAllAsText("a./*c*/"));
    assertEquals(List.of("a"), this.parseAllAsText("a.\t"));
    assertEquals(List.of("a", "b"), this.parseAllAsText("a.b."));
    assertEquals(List.of("X = \"a\""), this.parseAllAsText("X = \"a\". % end"));
  }

  @Test
  void testMissingFinalDotStopsParserAfterError() {
    final PrologParser parser = this.parseEd("a. b. c");
    assertEquals("a", parser.next().getText());
    assertEquals("b", parser.next().getText());
    assertTrue(parser.hasNext());

    final PrologParserException error = assertThrows(PrologParserException.class, parser::next);
    assertEquals("Non-closed clause", error.getMessage());
    assertFalse(parser.hasNext());
  }

  @Test
  void testPositionsAcrossLineEndings() {
    final PrologParser crlf = this.parseEd("a.\r\nb.\r\n\r\n  c.");
    this.assertPosition(crlf.next(), 1, 1);
    this.assertPosition(crlf.next(), 2, 1);
    this.assertPosition(crlf.next(), 4, 3);

    this.assertPosition(this.parseEd("\n\n\n  a.\n\n").next(), 4, 3);
    this.assertPosition(this.parseEd("\tfoo(\tX).").next(), 1, 2);
  }

  @Test
  void testPositionOfArgumentsOnContinuationLines() {
    final PrologTerm rule = this.parseEd("p :-\n  q,\n  r.").next();
    this.assertPosition(rule, 1, 3);
    final PrologTerm body = this.argAt(rule, 1);
    this.assertPosition(this.argAt(body, 0), 2, 3);
    this.assertPosition(this.argAt(body, 1), 3, 3);
  }

  @Test
  void testUnicodeAtomsAndVariables() {
    final PrologTerm cjk = this.rightOperand(this.parseEd("X = 日本.").next());
    assertEquals(TermType.ATOM, cjk.getType());
    assertEquals("日本", cjk.getText());

    assertEquals(TermType.VAR, this.rightOperand(this.parseEd("X = Δ.").next()).getType());
    assertEquals(TermType.ATOM, this.rightOperand(this.parseEd("X = δ.").next()).getType());

    final PrologTerm emoji = this.rightOperand(this.parseEd("X = '😀'.").next());
    assertEquals("😀", emoji.getText());
    assertEquals("X = '😀'", this.parseSingle("X = '😀'."));
    assertEquals("X = 'äöü'", this.parseSingle("X = 'äöü'."));
  }

  @Test
  void testEscapesInQuotedText() {
    assertEquals("aAb", this.quotedTextOf("X = 'a\\x41\\b'."));
    assertEquals("aA", this.quotedTextOf("X = 'a\\x41\\'."));
    assertEquals("A", this.quotedTextOf("X = '\\u0041'."));
    assertEquals("tab\there", this.quotedTextOf("X = 'tab\\there'."));
    assertEquals("a\\b", this.quotedTextOf("X = 'a\\\\b'."));
    assertEquals("abc\"def", this.quotedTextOf("X = \"abc\\\"def\"."));
    assertEquals("'", this.parseEd("'\\''.").next().getText());
    assertEquals("", this.quotedTextOf("X = \"\"."));
    assertEquals("", this.quotedTextOf("X = ``."));
  }

  @Test
  void testQuotationKindIsPreserved() {
    assertEquals(Quotation.DOUBLE, this.rightOperand(this.parseEd("X = \"s\".").next())
        .getQuotation());
    assertEquals(Quotation.BACK_TICK, this.rightOperand(this.parseEd("X = `s`.").next())
        .getQuotation());
    assertEquals(Quotation.SINGLE, this.rightOperand(this.parseEd("X = 's'.").next())
        .getQuotation());
  }

  @Test
  void testDeeplyNestedRedundantParenthesesCollapse() {
    final int depth = 500;
    final String source = "X = " + "(".repeat(depth) + "a" + ")".repeat(depth) + ".";
    PrologTerm current = this.rightOperand(this.parseEd(source).next());
    int blockWrappers = 0;
    while (current instanceof PrologStruct && ((PrologStruct) current).isAnyBlock()) {
      blockWrappers++;
      current = this.argAt(current, 0);
    }

    assertTrue(blockWrappers >= 1 && blockWrappers <= 2, "block wrappers: " + blockWrappers);
    assertEquals("a", current.getText());
    final String printed = this.parseSingle(source);
    assertEquals(printed, this.parseSingle(printed + "."));
  }

  @Test
  void testDeeplyNestedStructures() {
    final int depth = 500;
    final String source = "f(".repeat(depth) + "a" + ")".repeat(depth) + ".";
    PrologTerm current = this.parseEd(source).next();
    for (int level = 0; level < depth; level++) {
      assertEquals("f", current.getText(), "level " + level);
      current = this.argAt(current, 0);
    }
    assertEquals("a", current.getText());
  }

  @Test
  void testDeeplyNestedLists() {
    final int depth = 500;
    final String source = "[".repeat(depth) + "a" + "]".repeat(depth) + ".";
    PrologTerm current = this.parseEd(source).next();
    for (int level = 0; level < depth; level++) {
      assertInstanceOf(PrologList.class, current, "level " + level);
      current = ((PrologList) current).getHead();
    }
    assertEquals("a", current.getText());
  }

  @Test
  void testLongOperatorChains() {
    final int length = 1000;
    final String sum = Stream.generate(() -> "a").limit(length).collect(joining("+")) + ".";
    PrologTerm current = this.parseEd(sum).next();
    int plusCount = 0;
    while (current.getType() == TermType.STRUCT) {
      plusCount++;
      current = this.argAt(current, 0);
    }
    assertEquals(length - 1, plusCount);

    final String conjunction = Stream.generate(() -> "g").limit(length).collect(joining(",")) + ".";
    assertEquals(length, this.countConjuncts(this.parseEd(conjunction).next()));
  }

  @Test
  void testPrintedTermsParseBackToSameText() {
    final List<String> sources = List.of(
        "a :- b , c ; d -> e.",
        "X = - - 1.",
        "X = a - (-1).",
        "X = [a, b|T].",
        "X = '{}'(a).",
        "X = ','(a, b).",
        "X = f('-', '+').",
        "X = 'hello world'.",
        "X = \"q\\\"uote\".",
        "X = (a :- b) :- c.",
        "X = {a , b}.",
        "X = \\+ (a , b).",
        "f(A, _, _B, A).");

    for (final String source : sources) {
      final String printed = this.parseSingle(source);
      assertEquals(printed, this.parseSingle(printed + "."), source);
    }
  }

  @Test
  void testAtomLongerThanBufferLimitFails() {
    final ParserContext limited = this.withBufferLimit(16);
    final String longAtom = "a".repeat(64) + ".";
    assertThrows(CharBufferOverflowException.class,
        () -> new GenericPrologParser(new StringReader(longAtom), limited).next());
    assertEquals("a".repeat(15), new GenericPrologParser(
        new StringReader("a".repeat(15) + "."), limited).next().getText());
  }

  @Test
  void testQuotedAtomLongerThanBufferLimitFails() {
    final String quoted = "'" + "x".repeat(64) + "'.";
    assertThrows(CharBufferOverflowException.class,
        () -> new GenericPrologParser(new StringReader(quoted), this.withBufferLimit(16)).next());
  }

  @Test
  void testCommentsAreDeliveredToListenerAndSkippedInTerms() {
    final List<TokenizerResult> comments = new ArrayList<>();
    final PrologParser parser = new GenericPrologParser(
        new StringReader("a. % line\n/* block */ b(/*inner*/1)."),
        DefaultParserContext.of(EDINBURGH_FLAGS | FLAG_COMMENTS_AS_ATOMS, Op.SWI),
        List.of((source, comment) -> comments.add(comment)));

    assertEquals(List.of("a", "b(1)"),
        parser.stream().map(PrologTerm::toString).collect(toList()));
    assertEquals(List.of(Quotation.COMMENT_LINE, Quotation.COMMENT_BLOCK, Quotation.COMMENT_BLOCK),
        comments.stream().map(comment -> comment.getResult().getQuotation()).collect(toList()));
  }

  @Test
  void testCommentListenerIsSilentWithoutFlag() {
    final List<TokenizerResult> comments = new ArrayList<>();
    final PrologParser parser = new GenericPrologParser(
        new StringReader("a. % line\n/* block */ b."),
        DefaultParserContext.of(EDINBURGH_FLAGS, Op.SWI),
        List.of((source, comment) -> comments.add(comment)));

    assertEquals(2, parser.stream().count());
    assertTrue(comments.isEmpty());
  }

  @Test
  void testUserOperatorSharingPrefixWithBuiltIn() {
    final ParserContext context = DefaultParserContext.of(EDINBURGH_FLAGS, Op.SWI,
        Collections.singletonList(Op.make(700, XFX, "=:=:")));

    assertEquals("=:=:", this.parseWith("a =:=: b.", context).getText());
    assertEquals("=:=", this.parseWith("a =:= b.", context).getText());
    assertEquals("=:=", this.parseWith("a=:=b.", context).getText());
  }

  @Test
  void testMinusResolvesToPrefixOrInfixByPosition() {
    final PrologTerm prefix = this.parseEd("- a.").next();
    final PrologTerm infix = this.parseEd("b - a.").next();
    assertEquals(FY, ((Op) prefix.getFunctor()).getAssoc());
    assertEquals(YFX, ((Op) infix.getFunctor()).getAssoc());
  }

  private String parseSingle(final String source) {
    final PrologParser parser = this.parseEd(source);
    final PrologTerm term = parser.next();
    assertFalse(parser.hasNext(), () -> "Unexpected extra term in: " + source);
    return term.toString();
  }

  private List<String> parseAllAsText(final String source) {
    return this.parseEd(source).stream().map(PrologTerm::toString).collect(toList());
  }

  private PrologTerm parseWith(final String source, final ParserContext context) {
    return new GenericPrologParser(new StringReader(source), context).next();
  }

  private PrologParser parseWithFlags(final String source, final int flags) {
    return new GenericPrologParser(new StringReader(source),
        DefaultParserContext.of(flags, Op.SWI));
  }

  private PrologTerm argAt(final PrologTerm struct, final int index) {
    return ((PrologStruct) struct).getTermAt(index);
  }

  private PrologTerm rightOperand(final PrologTerm binary) {
    return this.argAt(binary, 1);
  }

  private BigInteger intAt(final PrologTerm struct, final int index) {
    return ((PrologInt) this.argAt(struct, index)).getIntValue();
  }

  private String quotedTextOf(final String source) {
    return this.rightOperand(this.parseEd(source).next()).getText();
  }

  private int countConjuncts(final PrologTerm term) {
    int count = 1;
    PrologTerm current = term;
    while (",".equals(current.getText()) && current.getType() == TermType.STRUCT) {
      count++;
      current = this.argAt(current, 1);
    }
    return count;
  }

  private void assertInteger(final long expected, final String source) {
    final PrologTerm term = this.parseEd(source).next();
    assertInstanceOf(PrologInt.class, term);
    assertEquals(BigInteger.valueOf(expected), ((PrologInt) term).getIntValue());
  }

  private void assertPosition(final PrologTerm term, final int line, final int pos) {
    assertEquals(line, term.getLine(), () -> "line of " + term);
    assertEquals(pos, term.getPos(), () -> "pos of " + term);
  }

  private void assertError(final String source, final String message, final int line,
                           final int pos) {
    final PrologParserException error =
        assertThrows(PrologParserException.class, () -> this.parseEd(source).next(), source);
    if (message != null) {
      assertEquals(message, error.getMessage(), source);
      assertEquals(line, error.getLine(), source);
      assertEquals(pos, error.getPos(), source);
    }
  }

  private ParserContext withBufferLimit(final int maxLength) {
    final ParserContext base = DefaultParserContext.of(EDINBURGH_FLAGS, Op.SWI);
    return new ParserContext() {
      @Override
      public boolean hasOpStartsWith(final PrologParser source, final String namePrefix) {
        return base.hasOpStartsWith(source, namePrefix);
      }

      @Override
      public OpContainer findOpForName(final PrologParser source, final String name) {
        return base.findOpForName(source, name);
      }

      @Override
      public int getMaxTokenizerBufferLength() {
        return maxLength;
      }

      @Override
      public int getFlags() {
        return base.getFlags();
      }
    };
  }
}
