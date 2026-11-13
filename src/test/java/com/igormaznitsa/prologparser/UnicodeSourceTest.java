package com.igormaznitsa.prologparser;

import static com.igormaznitsa.prologparser.ParserContext.FLAG_BLOCK_COMMENTS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_COMMENTS_AS_ATOMS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_CURLY_BRACKETS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_ZERO_QUOTATION_CHARCODE;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.FY;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFX;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.PrologInt;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.TokenizerResult;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;

class UnicodeSourceTest extends AbstractIntegrationTest {

  private static final int EDINBURGH_FLAGS =
      FLAG_BLOCK_COMMENTS | FLAG_ZERO_QUOTATION_CHARCODE | FLAG_CURLY_BRACKETS;

  @Test
  void testChineseFactHasChineseFunctorAndAtomArguments() {
    final PrologStruct fact = (PrologStruct) this.parseSingleTerm("父亲(张三, 李四).");

    assertEquals("父亲", fact.getFunctor().getText());
    assertEquals(2, fact.getArity());
    assertEquals(List.of("张三", "李四"), this.argumentTexts(fact));
    assertEquals(List.of(TermType.ATOM, TermType.ATOM), this.argumentTypes(fact));
  }

  @Test
  void testChineseRuleKeepsClauseStructure() {
    final PrologStruct rule =
        (PrologStruct) this.parseSingleTerm("祖父(X,Z) :- 父亲(X,Y), 父亲(Y,Z).");

    assertEquals(":-", rule.getFunctor().getText());
    assertEquals("祖父", rule.getTermAt(0).getText());
    assertEquals(",", rule.getTermAt(1).getText());
    assertEquals("祖父(X, Z) :- 父亲(X, Y) , 父亲(Y, Z)", rule.toString());
  }

  @Test
  void testChineseLettersJoinLatinLettersDigitsAndUnderscoreInOneAtom() {
    assertAll(Stream.of("abc中文", "中文abc", "中文_1", "中文123", "三", "日本語")
        .map(name -> () -> this.assertUnquotedAtom(name)));
  }

  @Test
  void testVariableNameMayContinueWithChineseLetters() {
    assertAll(Stream.of("X中", "_中文", "Name名字")
        .map(name -> () -> {
          final PrologTerm left = this.argAt(this.parseSingleTerm(name + " = 1."), 0);
          assertEquals(TermType.VAR, left.getType(), name);
          assertEquals(name, left.getText());
        }));
  }

  @Test
  void testOtherNonLatinScriptsReadAsUnquotedAtoms() {
    assertAll(Stream.of("ひらがな", "カタカナー", "한국어", "ไทย", "العربية", "привет", "café")
        .map(name -> () -> this.assertUnquotedAtom(name)));
  }

  @Test
  void testChineseInAllQuotationKindsIsPreservedVerbatim() {
    this.assertQuoted("X = '你好 世界'.", "你好 世界", Quotation.SINGLE, "X = '你好 世界'");
    this.assertQuoted("X = \"你好，世界\".", "你好，世界", Quotation.DOUBLE, "X = \"你好，世界\"");
    this.assertQuoted("X = `中文`.", "中文", Quotation.BACK_TICK, "X = `中文`");
    this.assertQuoted("X = '中\\n文'.", "中\n文", Quotation.SINGLE, "X = '中\\n文'");
  }

  @Test
  void testQuotationIsRequiredOnlyWhenChineseTextHasNonLetters() {
    assertEquals(Quotation.NONE, PrologTerm.findQuotation("中文"));
    assertEquals(Quotation.NONE, PrologTerm.findQuotation("中文_1"));
    assertEquals(Quotation.SINGLE, PrologTerm.findQuotation("你好，世界"));
    assertEquals(Quotation.SINGLE, PrologTerm.findQuotation("你好 世界"));
    assertEquals(Quotation.SINGLE, PrologTerm.findQuotation("1中文"));
  }

  @Test
  void testEscapesDecodeToChineseCharacters() {
    assertEquals("中", this.quotedTextOf("X = '\\u4E2D'."));
    assertEquals("中", this.quotedTextOf("X = '\\x4E2D\\'."));
    assertEquals("中文", this.quotedTextOf("X = \"\\u4E2D\\u6587\"."));
    assertEquals("中文", this.quotedTextOf("X = '\\x4E2D\\\\x6587\\'."));
  }

  @Test
  void testCharCodeOfChineseCharacter() {
    assertEquals(BigInteger.valueOf('中'), this.intAt(this.parseSingleTerm("X = 0'中."), 1));
    assertEquals(BigInteger.valueOf('中'), this.intAt(this.parseSingleTerm("X = 0'\\u4E2D."), 1));
  }

  @Test
  void testChineseCommentsAreSkipped() {
    assertEquals(List.of("foo(中)", "bar"),
        this.parseAllAsText("% 中文注释\nfoo(中). /* 块注释 中文 */ bar."));
  }

  @Test
  void testChineseCommentTextIsDeliveredToListener() {
    final List<TokenizerResult> comments = new ArrayList<>();
    final PrologParser parser = new GenericPrologParser(
        new StringReader("% 中文注释\nfoo. /* 块注释 */ bar."),
        DefaultParserContext.of(EDINBURGH_FLAGS | FLAG_COMMENTS_AS_ATOMS, Op.SWI),
        List.of((source, comment) -> comments.add(comment)));

    assertEquals(2, parser.stream().count());
    assertEquals(List.of(" 中文注释", " 块注释 "),
        comments.stream().map(comment -> comment.getResult().getText()).collect(toList()));
  }

  @Test
  void testIdeographicSpaceIsLayout() {
    assertEquals("X = 中文", this.parseSingle("X\u3000=\u3000中文."));
  }

  @Test
  void testChineseInListsAndCurlyBlocks() {
    assertEquals("X = [甲, 乙, 丙|尾]", this.parseSingle("X = [甲,乙,丙|尾]."));
    assertEquals("X = {中文}", this.parseSingle("X = {中文}."));
  }

  @Test
  void testChineseUserDefinedOperators() {
    final ParserContext context = DefaultParserContext.of(EDINBURGH_FLAGS, Op.SWI,
        List.of(Op.make(700, XFX, "等于"), Op.make(200, FY, "非")));

    final PrologTerm infix = this.parseWith("a 等于 b.", context);
    assertEquals(TermType.OPERATOR, infix.getFunctor().getType());
    assertEquals("等于", infix.getText());
    assertEquals("a 等于 b", this.parseWith("等于(a, b).", context).toString());
    assertEquals("非 a", this.parseWith("非 a.", context).toString());
  }

  @Test
  void testLetterOperatorIsNotSplitOutOfAdjacentLetters() {
    final ParserContext context = DefaultParserContext.of(EDINBURGH_FLAGS, Op.SWI,
        List.of(Op.make(700, XFX, "等于")));

    final PrologTerm glued = this.parseWith("a等于b.", context);
    assertEquals(TermType.ATOM, glued.getType());
    assertEquals("a等于b", glued.getText());
  }

  @Test
  void testPositionsCountEachChineseCharacterAsOneColumn() {
    final PrologTerm fact = this.parseSingleTerm("父亲(张三, 李四).");
    this.assertPosition(this.argAt(fact, 0), 1, 4);
    this.assertPosition(this.argAt(fact, 1), 1, 8);

    final List<PrologTerm> terms =
        this.parseEd("X = 'a', Y = 中.\n  Z = 文字(甲).").stream().collect(toList());
    this.assertPosition(this.argAt(this.argAt(terms.get(0), 1), 1), 1, 14);
    this.assertPosition(this.argAt(terms.get(1), 0), 2, 3);
    this.assertPosition(this.argAt(terms.get(1), 1), 2, 7);
  }

  @Test
  void testErrorPositionAfterChineseText() {
    final PrologParserException error =
        assertThrows(PrologParserException.class, () -> this.parseEd("父亲(张三 李四).").next());
    assertEquals(1, error.getLine());
    assertEquals(7, error.getPos());
  }

  @Test
  void testFullwidthPunctuationIsNotPrologPunctuation() {
    final PrologParserException comma =
        assertThrows(PrologParserException.class, () -> this.parseEd("X = 你好，世界.").next());
    assertEquals(7, comma.getPos());

    final PrologParserException fullStop =
        assertThrows(PrologParserException.class, () -> this.parseEd("a。").next());
    assertEquals("Non-closed clause", fullStop.getMessage());
  }

  @Test
  void testChineseProgramReadFromUtf8Source() throws IOException {
    try (Reader reader = new InputStreamReader(
        requireNonNull(this.getClass().getResourceAsStream("chinese_family.pl")), UTF_8)) {
      final List<String> clauses = this.parseEd(reader, DefaultParserContext.of(EDINBURGH_FLAGS))
          .stream()
          .map(PrologTerm::toString)
          .collect(toList());

      assertEquals(10, clauses.size());
      assertEquals(":- op(700, xfx, 是)", clauses.get(0));
      assertEquals("父亲(张三, 李四)", clauses.get(1));
      assertEquals("祖父(X, Z) :- 父亲(X, Y) , 父亲(Y, Z)", clauses.get(4));
      assertEquals("名字(张三, \"张 三\")", clauses.get(6));
      assertEquals("称呼(王五, '小王')", clauses.get(7));
      assertEquals("子女(张三, [李四, 李五|其他])", clauses.get(9));
    }
  }

  @Test
  void testQuotedSupplementaryCharactersArePreserved() {
    assertEquals("𠀀𠀁", this.quotedTextOf("X = '𠀀𠀁'."));
    assertEquals("😀", this.quotedTextOf("X = \"😀\"."));
  }

  @Test
  void testNonAsciiDecimalDigitsAreCurrentlyReadAsIntegers() {
    assertAll(Stream.of("١٢٣", "１２３", "12３")
        .map(digits -> () -> assertEquals(BigInteger.valueOf(123),
            this.intAt(this.parseSingleTerm("X = " + digits + "."), 1), digits)));
  }

  @Test
  void testNonBreakingSpaceIsNotLayout() {
    assertThrows(PrologParserException.class, () -> this.parseEd("X =\u00A0a.").next());
  }

  @Test
  void testTitlecaseLetterStartsAtom() {
    assertEquals(TermType.ATOM, this.argAt(this.parseSingleTerm("X = ǅ."), 1).getType());
    assertEquals(TermType.VAR, this.argAt(this.parseSingleTerm("X = Ωmega."), 1).getType());
  }

  @Test
  void testSupplementaryLetterIsPartOfUnquotedAtom() {
    this.assertUnquotedAtom("a𠀀");
    this.assertUnquotedAtom("𠀀");
    this.assertUnquotedAtom("𠀀a1");
    this.assertUnquotedAtom("𠀀𠀁");
  }

  @Test
  void testSupplementaryLetterContinuesVariableName() {
    final PrologTerm variable = this.argAt(this.parseSingleTerm("X𠀀 = 1."), 0);
    assertEquals(TermType.VAR, variable.getType());
    assertEquals("X𠀀", variable.getText());
  }

  @Test
  void testSupplementarySymbolSplitsFromLetters() {
    final PrologTerm emoji = this.argAt(this.parseSingleTerm("X = 😀."), 1);
    assertEquals("😀", emoji.getText());
    assertEquals(Quotation.SINGLE, emoji.getQuotation());
    assertThrows(PrologParserException.class, () -> this.parseEd("X = a😀.").next());
  }

  @Test
  void testSupplementaryCharacterOperatorIsUnreadCorrectly() {
    final ParserContext context = DefaultParserContext.of(EDINBURGH_FLAGS, Op.SWI,
        List.of(Op.make(700, XFX, "😀")));

    assertEquals("a 😀 b", this.parseWith("a😀b.", context).toString());
    assertEquals("1 😀 2", this.parseWith("1😀2.", context).toString());
    assertEquals("1.5 😀 X", this.parseWith("1.5😀X.", context).toString());
    assertEquals("X 😀 a", this.parseWith("X😀a.", context).toString());
  }

  @Test
  void testPositionsAfterSupplementaryCharacterCountUtf16Units() {
    final PrologTerm struct = this.parseSingleTerm("f(𠀀, b).");
    this.assertPosition(this.argAt(struct, 0), 1, 3);
    this.assertPosition(this.argAt(struct, 1), 1, 7);
  }

  @Test
  void testLoneSurrogateInSourceIsKeptAsIs() {
    assertEquals("\uD83Da", this.quotedTextOf("X = '\uD83Da'."));
    assertEquals("a\uDE00", this.quotedTextOf("X = 'a\uDE00'."));
  }

  @Test
  void testCharCodeOfSupplementaryCharacter() {
    assertEquals(BigInteger.valueOf(0x1F600), this.intAt(this.parseSingleTerm("X = 0'😀."), 1));
    assertEquals(BigInteger.valueOf(0x20000), this.intAt(this.parseSingleTerm("X = 0'𠀀."), 1));
  }

  @Test
  void testHexEscapeAboveBasicPlaneProducesCodePoint() {
    assertEquals("𠀀", this.quotedTextOf("X = '\\x20000\\'."));
    assertEquals("a😀b", this.quotedTextOf("X = \"a\\x1F600\\b\"."));
    assertEquals("𠀀", this.quotedTextOf("X = '\\400000\\'."));
  }

  @Test
  void testEscapeBeyondUnicodeRangeIsRejected() {
    assertThrows(PrologParserException.class, () -> this.parseEd("X = '\\x110000\\'.").next());
    assertThrows(PrologParserException.class, () -> this.parseEd("X = '\\4200000\\'.").next());
    assertThrows(PrologParserException.class, () -> this.parseEd("X = '\\UFFFFFFFF'.").next());
  }

  @Test
  void testLongUnicodeEscape() {
    assertEquals("😀", this.quotedTextOf("X = '\\U0001F600'."));
    assertEquals("中", this.quotedTextOf("X = '\\U00004E2D'."));
    assertEquals(BigInteger.valueOf(0x1F600),
        this.intAt(this.parseSingleTerm("X = 0'\\U0001F600."), 1));
  }

  @Test
  void testShortOrMalformedLongUnicodeEscapeIsRejected() {
    assertThrows(PrologParserException.class, () -> this.parseEd("X = '\\U1F600'.").next());
    assertThrows(PrologParserException.class, () -> this.parseEd("X = '\\U0001F60G'.").next());
  }

  @Test
  void testCombiningMarksStayInsideAtom() {
    this.assertUnquotedAtom("हिन्दी");
    this.assertUnquotedAtom("e\u0301");
    this.assertUnquotedAtom("a\u20DD");
  }

  @Test
  void testCombiningMarksStayInsideVariableName() {
    final PrologTerm variable = this.argAt(this.parseSingleTerm("E\u0301 = 1."), 0);
    assertEquals(TermType.VAR, variable.getType());
    assertEquals("E\u0301", variable.getText());
  }

  @Test
  void testQuotationForSupplementaryAndCombiningText() {
    assertEquals(Quotation.NONE, PrologTerm.findQuotation("a𠀀"));
    assertEquals(Quotation.NONE, PrologTerm.findQuotation("𠀀"));
    assertEquals(Quotation.NONE, PrologTerm.findQuotation("e\u0301"));
    assertEquals(Quotation.SINGLE, PrologTerm.findQuotation("😀"));
    assertEquals(Quotation.SINGLE, PrologTerm.findQuotation("a😀"));
    assertEquals(Quotation.SINGLE, PrologTerm.findQuotation("\u0301e"));
    assertEquals(Quotation.SINGLE, PrologTerm.findQuotation("\uD83Da"));
  }

  private void assertUnquotedAtom(final String name) {
    final PrologTerm atom = this.argAt(this.parseSingleTerm("X = " + name + "."), 1);
    assertEquals(TermType.ATOM, atom.getType(), name);
    assertEquals(name, atom.getText());
    assertEquals(Quotation.NONE, atom.getQuotation(), name);
    assertEquals(name, atom.toString(), name);
  }

  private void assertQuoted(final String source, final String text, final Quotation quotation,
                            final String printed) {
    final PrologTerm atom = this.argAt(this.parseSingleTerm(source), 1);
    assertEquals(text, atom.getText(), source);
    assertEquals(quotation, atom.getQuotation(), source);
    assertEquals(printed, this.parseSingle(source));
    assertEquals(printed, this.parseSingle(printed + "."));
  }

  private PrologTerm parseSingleTerm(final String source) {
    final PrologParser parser = this.parseEd(source);
    final PrologTerm term = parser.next();
    assertFalse(parser.hasNext(), () -> "Unexpected extra term in: " + source);
    return term;
  }

  private String parseSingle(final String source) {
    return this.parseSingleTerm(source).toString();
  }

  private List<String> parseAllAsText(final String source) {
    return this.parseEd(source).stream().map(PrologTerm::toString).collect(toList());
  }

  private PrologTerm parseWith(final String source, final ParserContext context) {
    return new GenericPrologParser(new StringReader(source), context).next();
  }

  private String quotedTextOf(final String source) {
    return this.argAt(this.parseSingleTerm(source), 1).getText();
  }

  private PrologTerm argAt(final PrologTerm struct, final int index) {
    return ((PrologStruct) struct).getTermAt(index);
  }

  private BigInteger intAt(final PrologTerm struct, final int index) {
    return assertInstanceOf(PrologInt.class, this.argAt(struct, index)).getIntValue();
  }

  private List<String> argumentTexts(final PrologStruct struct) {
    return IntStream.range(0, struct.getArity())
        .mapToObj(index -> struct.getTermAt(index).getText())
        .collect(toList());
  }

  private List<TermType> argumentTypes(final PrologStruct struct) {
    return IntStream.range(0, struct.getArity())
        .mapToObj(index -> struct.getTermAt(index).getType())
        .collect(toList());
  }

  private void assertPosition(final PrologTerm term, final int line, final int pos) {
    assertEquals(line, term.getLine(), () -> "line of " + term);
    assertEquals(pos, term.getPos(), () -> "pos of " + term);
  }
}
