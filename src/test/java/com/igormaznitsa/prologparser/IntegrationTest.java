package com.igormaznitsa.prologparser;

import static com.igormaznitsa.prologparser.DefaultParserContext.of;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_BLOCK_COMMENTS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_CURLY_BRACKETS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_NONE;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_VAR_AS_FUNCTOR;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_ZERO_STRUCT;
import static com.igormaznitsa.prologparser.terms.OpContainer.make;
import static com.igormaznitsa.prologparser.terms.Quotation.BACK_TICK;
import static com.igormaznitsa.prologparser.terms.Quotation.DOUBLE;
import static com.igormaznitsa.prologparser.terms.Quotation.NONE;
import static com.igormaznitsa.prologparser.terms.Quotation.SINGLE;
import static com.igormaznitsa.prologparser.terms.TermType.ATOM;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.FX;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.FY;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XF;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFX;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFY;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.YFX;
import static java.util.stream.Collectors.joining;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloat;
import com.igormaznitsa.prologparser.terms.PrologInt;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologNumeric;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.PrologVar;
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import com.igormaznitsa.prologparser.tokenizer.TokenizerResult;
import com.igormaznitsa.prologparser.utils.StringUtils;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;

@SuppressWarnings({"varargs", "UnnecessaryUnicodeEscape", "OptionalGetWithoutIsPresent",
    "resource"})
class IntegrationTest extends AbstractIntegrationTest {

  @Test
  void testReadingClausesTokensAndCharsFromSameParser() throws Exception {
    try (final PrologParser parser = parseEd("lsome(a). alone. next(b).c")) {
      assertTrue(parser.hasNext());
      final PrologTerm term1 = parser.next();
      assertEquals("lsome(a)", term1.toString());
      assertTrue(parser.hasNext());
      final TokenizerResult token1 = parser.getInternalTokenizer().readNextToken();
      assertEquals("alone", Objects.requireNonNull(token1).getResult().toString());
      assertTrue(parser.hasNext());
      final TokenizerResult token2 = parser.getInternalTokenizer().readNextToken();
      assertEquals(".", Objects.requireNonNull(token2).getResult().getText());
      assertTrue(parser.hasNext());
      final PrologTerm term2 = parser.next();
      assertEquals("next(b)", term2.toString());
      assertTrue(parser.hasNext());
      assertThrows(PrologParserException.class, parser::next);
    }
  }

  @Test
  void testEmptyAndNonClause() {
    assertThrows(NoSuchElementException.class, () -> parseEd("").next());
    assertThrows(NoSuchElementException.class, () -> parseEd(".").next());
    assertThrows(PrologParserException.class, () -> parseEd("ab").next());
    assertThrows(PrologParserException.class, () -> parseEd("1").next());
  }

  @Test
  void testDot2StructAsList() {
    assertEquals("'.'(a, [])", parseGen("'.'(a,[]).").next().toString());
    assertEquals("'.'(a, X)", parseGen("'.'(a,X).").next().toString());
    assertEquals("'.'(a, b)", parseGen("'.'(a,b).").next().toString());

    assertEquals("[a]", parseIso("'.'(a,[]).").next().toString());
    assertEquals("[a|X]", parseIso("'.'(a,X).").next().toString());
    assertEquals("[a|b]", parseIso("'.'(a,b).").next().toString());

    assertEquals("[a, b, c]", parseIso("'.'(a,'.'(b,'.'(c,[]))).").next().toString());
    assertEquals("[a, b, c|X]", parseIso("'.'(a,'.'(b,'.'(c,X))).").next().toString());
  }

  @Test
  void testSwiCpl() {
    assertEquals("X in 5 .. 10 , 5 , Y #=< X + -1 , 6 , Y in 4 .. 8",
        parseCpl("X in 5..10,5,Y#=<X+ -1,6,Y in 4..8.").next().toString());
    assertEquals("#\\ X", parseCpl("#\\X.").next().toString());
    assertEquals("M #\\= 0 , S #\\= 0", parseCpl("M #\\= 0, S #\\= 0.").next().toString());
    assertEquals("Temp #< 800 , #\\ Startup #==> Temp #> 300",
        parseCpl("Temp#<800,#\\Startup#==>Temp#>300.").next().toString());
    assertEquals("A in 1 \\/ 3 \\/ 5 \\/ 8 #==> B #=< 8 , A in 9 .. 11 \\/ 14 #==> B #> 8",
        parseCpl("A in 1\\/3\\/5\\/8#==>B#=<8,A in 9..11\\/14#==>B#>8.").next().toString());
    assertEquals("[X, Y] ins 1 .. 3 , labeling([max(X), min(Y)], [X, Y])",
        parseCpl("[X,Y] ins 1..3,labeling([max(X),min(Y)],[X,Y]).").next().toString());
    assertEquals(
        "sort_of_near_to(X, Y, N, M) :- X #> Y #==> X - N #>= Y #/\\ X - M - 1 #< Y , X #=< Y #==> Y - N #>= X #/\\ Y - M - 1 #< X",
        parseCpl(
            "sort_of_near_to(X, Y, N, M):-X#>Y#==>X-N#>=Y#/\\X-M-1#<Y,X#=<Y#==>Y-N#>=X#/\\Y-M-1#<X.").next()
            .toString());
    assertEquals("V in inf .. -2 \\/ 0 \\/ 2 .. 16 \\/ 18 .. sup",
        parseCpl("V in inf.. -2 \\/ 0 \\/ 2..16 \\/ 18..sup.").next().toString());
    assertEquals(
        "X #> Y #==> X - N #>= Y #/\\ X - M - 1 #< Y , X #=< Y #==> Y - N #>= X #/\\ Y - M - 1 #< X",
        parseCpl("X#>Y#==>X-N#>=Y#/\\X-M-1#<Y,X #=< Y #==> Y - N #>= X #/\\ Y - M - 1 #< X.").next()
            .toString());
    assertEquals(
        "clpfd : run_propagator(even(X), MState) :- (integer(X) -> clpfd : kill(MState) , 0 is X mod 2 ; true)",
        parseCpl(
            "clpfd:run_propagator(even(X),MState):-(integer(X)->clpfd:kill(MState),0is X mod 2;true).").next()
            .toString());
    assertEquals("#\\ X .. (#\\ Y)", parseCpl("#\\X..#\\Y.").next().toString());
  }

  @Test
  void testParseStringWithSpecialChars() {
    final PrologParser parser =
        parseEd("'0\\'a Hello\\\nWorld\u0021\\r'.\"0'a \\xFF\\Another String\".");
    PrologTerm term = parser.next();

    assertEquals(ATOM, term.getType());
    assertEquals(Quotation.SINGLE, term.getQuotation());
    assertEquals("0\\'a Hello\\nWorld!\\r", StringUtils.escapeString(term.getText(), SINGLE));

    term = parser.next();
    assertNotNull(term);
    assertEquals(Quotation.DOUBLE, term.getQuotation());
    assertEquals("0'a ÿAnother String", term.getText());

  }

  private void assertPrologInteger(final long expected, final String parse) {
    final PrologParser parser = parseEd(parse);
    final PrologTerm term = parser.next();
    assertFalse(parser.hasNext());
    assertEquals(TermType.ATOM, term.getType());
    assertEquals(1, term.getLine());
    assertEquals(1, term.getPos());
    assertTrue(term instanceof PrologInt);
    assertEquals(expected, ((PrologInt) term).getNumber().longValue());
  }

  @Test
  void testParseZeroSingleQuoteChar() {
    assertPrologInteger('a', "0'a.");
    assertPrologInteger(' ', "0'\\s.");
    assertPrologInteger('\u1234', "0'\\u1234.");
    assertPrologInteger('\n', "0'\\n.");
    assertPrologInteger('\"', "0'\".");
    assertPrologInteger('\'', "0'\\'.");
    assertPrologInteger('`', "0'`.");
    checkWrongClauseReadingWithPPE("0'ab.", 4);
  }

  @Test
  void testVariableMustBeNotEqualAtClauseBounds() {
    PrologStruct structure = (PrologStruct) parseEd(
        "test(A,B,C,A,B,C,A,B,C,A,B,C,_,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,_,_).").next();

    final Set<PrologVar> varSet = new HashSet<>();
    for (int li = 0; li < structure.getArity(); li++) {
      final PrologVar currentVar = (PrologVar) structure.getTermAt(li);
      assertFalse(varSet.contains(currentVar));
      varSet.add(currentVar);
    }

    assertEquals(structure.getArity(), varSet.size());
  }

  @Test
  void testVariablesAtClauseBounds() {
    PrologStruct structure = (PrologStruct) parseEd(
        "test(A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C).").next();

    final PrologVar varA = (PrologVar) structure.getTermAt(0);
    final PrologVar varB = (PrologVar) structure.getTermAt(1);
    final PrologVar varC = (PrologVar) structure.getTermAt(2);

    assertNotSame(varA, varB);
    assertNotSame(varA, varC);
    assertNotSame(varB, varC);

    assertEquals("A", varA.getText());
    assertEquals("B", varB.getText());
    assertEquals("C", varC.getText());

    for (int li = 3; li < structure.getArity(); ) {
      assertNotSame(varA, structure.getTermAt(li++));
      assertNotSame(varB, structure.getTermAt(li++));
      assertNotSame(varC, structure.getTermAt(li++));
    }
  }

  @Test
  void testEndOfStream() {
    final PrologParser parser = parseEd("hello.world.");
    PrologTerm term = parser.next();

    assertEquals(ATOM, term.getType());
    assertEquals("hello", term.getText());

    term = parser.next();
    assertEquals(ATOM, term.getType());
    assertEquals("world", term.getText());

    assertThrows(NoSuchElementException.class, parser::next);
  }

  private void checkWrongClauseReadingWithPPE(final String readClause, final int stringPosition) {
    assertEquals(stringPosition,
        assertThrows(PrologParserException.class, () -> parseEd(readClause).next()).getPos());
  }

  @Test
  void testErrorListDefinitions() {
    checkWrongClauseReadingWithPPE(" [,].", 3);
    checkWrongClauseReadingWithPPE(" [|].", 2);
    checkWrongClauseReadingWithPPE(" [345|].", 7);
    checkWrongClauseReadingWithPPE(" [345|323|X].", 11);
    checkWrongClauseReadingWithPPE(" [345|323|].", 10);
    checkWrongClauseReadingWithPPE(" [|345].", 2);
    checkWrongClauseReadingWithPPE(" [1,2,3.", 8);
  }

  @Test
  void testQuoting_Wrong() {
    checkWrongClauseReadingWithPPE("\"abc'.", 1);
    checkWrongClauseReadingWithPPE("'abc\".", 1);
    checkWrongClauseReadingWithPPE("`abc\".", 1);
    checkWrongClauseReadingWithPPE("`abc'.", 1);
  }

  @Test
  void testQuoting() {
    checkParseAtomQuoting("test.", "test", NONE);
    checkParseAtomQuoting("'t\"es`t'.", "'t\"es`t'", SINGLE);
    checkParseAtomQuoting("`t'\\`e's\"t`.", "`t'\\`e's\"t`", BACK_TICK);
    checkParseAtomQuoting("\"t`e's\\\"\".", "\"t`e's\\\"\"", DOUBLE);
  }

  private void checkParseAtomWithoutPPE(final String atomToBeChecked,
                                        final String expectedAtomText) {
    final PrologTerm atom = parseEd(atomToBeChecked + '.').next();
    assertEquals(ATOM, atom.getType());
    assertEquals(PrologAtom.class, atom.getClass());
    assertEquals(expectedAtomText, atom.getText());
  }

  private void checkParseAtomQuoting(final String atomToBeChecked, final String expectedAtomText,
                                     final Quotation expectedType) {
    final PrologTerm atom = parseEd(atomToBeChecked + '.').next();
    assertEquals(ATOM, atom.getType());
    assertEquals(expectedType, atom.getQuotation());
    assertEquals(PrologAtom.class, atom.getClass());
    assertEquals(expectedAtomText, atom.toString());
  }

  @Test
  void testParseAtom() {
    checkParseAtomWithoutPPE("a", "a");
    checkParseAtomWithoutPPE("test012", "test012");
    checkParseAtomWithoutPPE("x______y", "x______y");
    checkParseAtomWithoutPPE("alpha_beta_procedure", "alpha_beta_procedure");
    // test of non-latin chars, "hello" in russian
    checkParseAtomWithoutPPE("привет", "привет");
    checkParseAtomWithoutPPE("miss_Jones", "miss_Jones");
    checkParseAtomWithoutPPE("'Jones'", "Jones");
    checkParseAtomWithoutPPE("''", "");
    checkParseAtomWithoutPPE("x_", "x_");
  }

  @Test
  void testParseInteger() {
    assertThrows(PrologParserException.class, () -> parseEd("1_000_/*more*/000").next());
    checkIntegerWithoutPPE("-17", -17);
    checkIntegerWithoutPPE("0", 0);
    checkIntegerWithoutPPE("1", 1);
    checkIntegerWithoutPPE("1313", 1313);
    checkIntegerWithoutPPE("-97", -97);
    checkIntegerWithoutPPE(Long.toString(Long.MAX_VALUE), Long.MAX_VALUE);

    checkIntegerWithoutPPE(Long.toString(Long.MIN_VALUE), Long.MIN_VALUE);

    final PrologTerm val = parseEd("'298723987'.").next();
    assertEquals(ATOM, val.getType());
    assertEquals(PrologAtom.class, val.getClass());
    assertEquals("298723987", val.getText());
  }

  private void checkFloatWithoutPPE(final String atomToBeChecked, final double expectedNumber) {
    final PrologTerm atom = parseEd(atomToBeChecked + '.').next();
    assertEquals(ATOM, atom.getType());
    assertEquals(PrologFloat.class, atom.getClass());
    assertEquals(expectedNumber, ((PrologFloat) atom).getNumber().doubleValue(), Double.MIN_NORMAL,
        String.format("%e <> %e", expectedNumber, ((PrologFloat) atom).getNumber().doubleValue()));
    assertEquals(BigDecimal.valueOf(expectedNumber).toEngineeringString(), atom.getText());
  }

  @Test
  void testParseFloat() {
    checkFloatWithoutPPE(new BigDecimal(Math.PI, PrologFloat.MATH_CONTEXT).toEngineeringString(),
        Math.PI);
    checkFloatWithoutPPE("-2.67e+021", -2.67e+021);
    checkFloatWithoutPPE("-0.0035", -0.0035d);
    checkFloatWithoutPPE("100.2", 100.2d);
    checkFloatWithoutPPE("2000.0", 2.0e+3d);

    final PrologTerm val = parseEd("298723987493287423423.00002342342300043324234324E+75.").next();
    assertEquals(ATOM, val.getType());
    assertEquals(PrologFloat.class, val.getClass());
  }

  @Test
  void testParseVariable() {
    PrologTerm var = parseEd("X.").next();
    assertEquals(TermType.VAR, var.getType());
    assertEquals(PrologVar.class, var.getClass());
    assertEquals("X", var.getText());

    var = parseEd("Result.").next();
    assertEquals(TermType.VAR, var.getType());
    assertEquals(PrologVar.class, var.getClass());
    assertEquals("Result", var.getText());

    var = parseEd("Object2.").next();
    assertEquals(TermType.VAR, var.getType());
    assertEquals(PrologVar.class, var.getClass());
    assertEquals("Object2", var.getText());

    var = parseEd("Participant_list.").next();
    assertEquals(TermType.VAR, var.getType());
    assertEquals(PrologVar.class, var.getClass());
    assertEquals("Participant_list", var.getText());

    var = parseEd("ShoppingList.").next();
    assertEquals(TermType.VAR, var.getType());
    assertEquals(PrologVar.class, var.getClass());
    assertEquals("ShoppingList", var.getText());

    var = parseEd("_x23.").next();
    assertEquals(TermType.VAR, var.getType());
    assertEquals(PrologVar.class, var.getClass());
    assertEquals("_x23", var.getText());

    var = parseEd("_23.").next();
    assertEquals(TermType.VAR, var.getType());
    assertEquals(PrologVar.class, var.getClass());
    assertEquals("_23", var.getText());
  }

  @Test
  void testParseStructure() {
    final ParserContext mockContext = mock(ParserContext.class);
    when(mockContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    PrologParser parser = parseEd("date(Day,may,2001).", mockContext);
    final PrologTerm term = parser.next();

    assertEquals(TermType.STRUCT, term.getType());

    PrologStruct struct = (PrologStruct) term;
    assertEquals(ATOM, struct.getFunctor().getType());
    assertEquals("date", struct.getFunctor().getText());
    assertEquals(3, struct.getArity());
    assertEquals(TermType.VAR, struct.getTermAt(0).getType());
    assertEquals("Day", struct.getTermAt(0).getText());
    assertEquals(ATOM, struct.getTermAt(1).getType());
    assertEquals("may", struct.getTermAt(1).getText());
    assertEquals(ATOM, struct.getTermAt(2).getType());
    assertEquals(2001L,
        ((PrologInt) struct.getTermAt(2)).getNumber().longValue());
  }

  @Test
  void testParseList() {
    assertThrows(PrologParserException.class, () -> parseEd("[1,2|3,4].").next());
    assertThrows(PrologParserException.class, () -> parseEd("[1,2|3,4,5].").next());
    assertThrows(PrologParserException.class, () -> parseEd("[1,2|3|4].").next());
    assertThrows(PrologParserException.class, () -> parseEd("[1,2||4].").next());
    assertThrows(PrologParserException.class, () -> parseEd("[|4].").next());

    PrologList list = (PrologList) parseEd("[].").next();
    assertEquals(TermType.LIST, list.getType());
    assertEquals(PrologList.class, list.getClass());
    assertTrue((list).isEmpty());

    list = (PrologList) parseEd("[1,2,-3,4,5,-5].").next();
    assertFalse(list.isEmpty());
    assertEquals("[1, 2, -3, 4, 5, -5]", list.toString());

    list = (PrologList) parseEd("[Hello|World].").next();
    assertFalse(list.isEmpty());
    assertEquals("[Hello|World]", list.toString());
    assertEquals(TermType.VAR, list.getHead().getType());
    assertEquals("Hello", list.getHead().getText());
    assertEquals(TermType.VAR, list.getTail().getType());
    assertEquals("World", list.getTail().getText());

    list = (PrologList) parseEd("[a|[b,c]].").next();
    assertFalse(list.isEmpty());
    assertEquals("[a, b, c]", list.toString());

    list = (PrologList) parseEd("[a,b|[c]].").next();
    assertFalse(list.isEmpty());
    assertEquals("[a, b, c]", list.toString());

    list = (PrologList) parseEd("[a,b,c|[]].").next();
    assertFalse(list.isEmpty());
    assertEquals("[a, b, c]", list.toString());
  }

  @Test
  void testParseOperator() {
    final ParserContext mockContext = mock(ParserContext.class);
    when(mockContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    PrologParser parser = parseEd("hello:-world.", mockContext);

    PrologStruct struct = (PrologStruct) parser.next();

    assertEquals(PrologStruct.class, struct.getClass());
    assertEquals(TermType.OPERATOR, struct.getFunctor().getType());
    assertEquals(":-", struct.getFunctor().getText());
    assertEquals(2, struct.getArity());
    assertEquals(OpAssoc.XFX,
        ((Op) struct.getFunctor()).getAssoc());
    assertEquals("hello", struct.getTermAt(0).getText());
    assertEquals("world", struct.getTermAt(1).getText());

    clearInvocations(mockContext);
    parser = parseEd(":-test.", mockContext);
    struct = (PrologStruct) parser.next();

    assertEquals(PrologStruct.class, struct.getClass());
    assertEquals(TermType.OPERATOR, struct.getFunctor().getType());
    assertEquals(":-", struct.getFunctor().getText());
    assertEquals(1, struct.getArity());
    assertEquals(OpAssoc.FX,
        ((Op) struct.getFunctor()).getAssoc());
    assertEquals("test", struct.getTermAt(0).getText());

    clearInvocations(mockContext);
    parser = parseEd("X is X+1.", mockContext);
    struct = (PrologStruct) parser.next();

    assertEquals(PrologStruct.class, struct.getClass());
    assertEquals(TermType.OPERATOR, struct.getFunctor().getType());
    assertEquals("is", struct.getFunctor().getText());
    assertEquals(2, struct.getArity());
    assertEquals(OpAssoc.XFX,
        ((Op) struct.getFunctor()).getAssoc());
    assertEquals("X", struct.getTermAt(0).getText());
    assertEquals(TermType.STRUCT, struct.getTermAt(1).getType());
    assertEquals("+", struct.getTermAt(1).getFunctor().getText());
    assertEquals(OpAssoc.YFX,
        ((Op) struct.getTermAt(1).getFunctor()).getAssoc());
    assertEquals(2, struct.getTermAt(1).getArity());
    assertEquals("X", ((PrologStruct) struct.getTermAt(1)).getTermAt(0).getText());
    assertEquals("1", ((PrologStruct) struct.getTermAt(1)).getTermAt(1).getText());

    assertEquals("+ - + a", parseEd("+-+a.").next().toString());
    assertEquals("+ - + 1", parseEd("+-+1.").next().toString());
    assertEquals("+ -1", parseEd("+-1.").next().toString());
    assertEquals("X is 2 + 3 * 5", parseEd("X is 2+3*5.").next().toString());
  }

  @Test
  void testComments() {
    final PrologParser parser = parseEd(
        "  %   zero line %%%% misc%%%% \n%first line\n%second line\n\nhello:-world.%test\n:-test.");
    PrologTerm term = parser.next();
    PrologTerm term2 = parser.next();

    assertEquals("hello :- world", term.toString());
    assertEquals(":- test", term2.toString());

    assertFalse(parser.hasNext());
  }

  @Test
  void testSimilarOperatorInterpretation() {
    final Map<String, OpContainer> operators = new HashMap<>();
    final Op firstOperator = Op.make(100, OpAssoc.FY, "++++++");
    final Op secondOperator = Op.make(100, OpAssoc.FY, "+++");
    final Op thirdOperator = Op.make(100, OpAssoc.FY, "++");

    operators.put(firstOperator.getText(), make(firstOperator));
    operators.put(secondOperator.getText(), make(secondOperator));
    operators.put(thirdOperator.getText(), make(thirdOperator));

    final StubContext contextStub = new StubContext(operators);
    final PrologTerm term = parseEd("+++++++++++ 3.", contextStub).next();

    assertSame(firstOperator, term.getFunctor());
    assertSame(secondOperator, ((PrologStruct) term).getTermAt(0).getFunctor());
    assertSame(thirdOperator,
        ((PrologStruct) (((PrologStruct) term).getTermAt(0))).getTermAt(0).getFunctor());

  }

  @Test
  void testOperatorHierarchy() {
    final Map<String, OpContainer> operators = new HashMap<>();

    final StubContext contextStub = new StubContext(operators);
    final PrologParser parser = parseEd(
        ":-op(800,xfx,'<===>').:-op(700,xfy,'v').:-op(600,xfy,'&').:-op(500,fy,'~').~(A&B)<===> ~A v ~B.",
        contextStub);

    PrologTerm term = null;

    while (parser.hasNext()) {
      term = parser.next();

      if (term.getType() == TermType.STRUCT) {
        final PrologStruct structure = (PrologStruct) term;
        if (structure.getArity() == 1 && structure.getFunctor().getText().equals(":-")) {
          final PrologStruct operatorstructure = (PrologStruct) structure.getTermAt(0);

          if (operatorstructure.getArity() == 3 &&
              operatorstructure.getFunctor().getText().equals("op")) {
            final Op newoperator = Op.make(
                ((PrologInt) operatorstructure.getTermAt(0)).getNumber().intValue(),
                OpAssoc.findForName(operatorstructure.getTermAt(1).getText()).get(),
                operatorstructure.getTermAt(2).getText());

            OpContainer container = operators.get(newoperator.getText());

            if (container == null) {
              container = make(newoperator);
              operators.put(newoperator.getText(), container);
            } else {
              container.add(newoperator);
            }

          } else {
            fail("Unsupported structure detected");
          }
        }
      }
    }

    assertEquals(TermType.STRUCT, Objects.requireNonNull(term).getType());
    assertEquals("<===>", term.getText());
    assertEquals(2, term.getArity());
    assertEquals(800, term.getPrecedence());

    final PrologStruct leftBranch = (PrologStruct) ((PrologStruct) term).getTermAt(0);
    final PrologStruct rightBranch = (PrologStruct) ((PrologStruct) term).getTermAt(1);

    assertEquals(500, leftBranch.getPrecedence());
    assertEquals("~", leftBranch.getText());
    assertEquals(1, leftBranch.getArity());

    assertEquals(700, rightBranch.getPrecedence());
    assertEquals("v", rightBranch.getText());
    assertEquals(2, rightBranch.getArity());

    final PrologStruct leftBranch2 = (PrologStruct) leftBranch.getTermAt(0);
    final PrologStruct rightBranchL = (PrologStruct) rightBranch.getTermAt(0);
    final PrologStruct rightBranchR = (PrologStruct) rightBranch.getTermAt(1);

    assertEquals(600, leftBranch2.getPrecedence());
    assertEquals("&", leftBranch2.getText());
    assertEquals(2, leftBranch2.getArity());
    assertEquals("A", leftBranch2.getTermAt(0).getText());
    assertEquals("B", leftBranch2.getTermAt(1).getText());

    assertEquals(500, rightBranchL.getPrecedence());
    assertEquals("~", rightBranchL.getText());
    assertEquals(1, rightBranchL.getArity());
    assertEquals("A", rightBranchL.getTermAt(0).getText());

    assertEquals(500, rightBranchR.getPrecedence());
    assertEquals("~", rightBranchR.getText());
    assertEquals(1, rightBranchR.getArity());
    assertEquals("B", rightBranchR.getTermAt(0).getText());

    assertEquals("~ (A & B) <===> ~ A v ~ B", term.toString());
  }

  @Test
  void testCurlyBracket() {
    assertEquals("{1 , 2 , 3 , 4 , 5}",
        new GenericPrologParser(new StringReader("{1,2,3,4,5}."), of(FLAG_CURLY_BRACKETS)).next()
            .toString());
    assertEquals("{1 , {2 , {3 , {4} , 5}}}",
        new GenericPrologParser(new StringReader("{1,{2,{3,{4},5}}}."),
            of(FLAG_CURLY_BRACKETS)).next().toString());
    assertEquals("[1, {2 , 3}|X]",
        new GenericPrologParser(new StringReader("[1,{2,3}|X]."), of(FLAG_CURLY_BRACKETS)).next()
            .toString());
    assertEquals("{}",
        new GenericPrologParser(new StringReader("{}."), of(FLAG_CURLY_BRACKETS)).next()
            .toString());

    assertThrows(PrologParserException.class,
        () -> new GenericPrologParser(new StringReader("test{1,2,3,4,5}."),
            of(FLAG_CURLY_BRACKETS)).next());
  }

  @Test
  void testOperatorExamplesFromSictusManual() {
    final ParserContext sictusContext = makeSictusContext();

    PrologParser parser = new GenericPrologParser(new StringReader(":-(p;q),r."), sictusContext);

    PrologTerm term = parser.next();
    assertFalse(parser.hasNext());

    assertEquals(":-", term.getFunctor().getText());
    assertEquals(1, term.getArity());
    term = ((PrologStruct) term).getTermAt(0);
    assertEquals(",", term.getFunctor().getText());
    assertEquals("r", ((PrologStruct) term).getTermAt(1).getText());
    term = ((PrologStruct) term).getTermAt(0);
    assertEquals("()", term.getFunctor().getText());
    term = ((PrologStruct) term).getTermAt(0);
    assertEquals(";", term.getFunctor().getText());
    assertEquals("p", ((PrologStruct) term).getTermAt(0).getText());
    assertEquals("q", ((PrologStruct) term).getTermAt(1).getText());
  }

  @Test
  void testParseSictusBench() {
    assertReadSictusTerms(136, "boyer.pl");
    assertReadSictusTerms(29, "browse.pl");
    assertReadSictusTerms(518, "chat_parser.pl");
    assertReadSictusTerms(30, "crypt.pl");
    assertReadSictusTerms(17, "deriv.pl");
    assertReadSictusTerms(9, "dynamic_unit_clause.pl");
    assertReadSictusTerms(17, "fast_mu.pl");
    assertReadSictusTerms(57, "flatten.pl");
    assertReadSictusTerms(10, "harness.pl");
    assertReadSictusTerms(5, "itak.pl");
    assertReadSictusTerms(27, "main.pl");
    assertReadSictusTerms(27, "meta_qsort.pl");
    assertReadSictusTerms(18, "mu.pl");
    assertReadSictusTerms(7, "nreverse.pl");
    assertReadSictusTerms(5, "nreverse_builtin.pl");
    assertReadSictusTerms(34, "poly.pl");
    assertReadSictusTerms(11, "primes.pl");

    assertReadSictusTerms(39, "prover.pl", Op.make(950, XFY, "#"), Op.make(850, XFY, "&"),
        Op.make(500, FX, "-", "+"));
    assertReadSictusTerms(8, "qsort.pl");
    assertReadSictusTerms(14, "queens.pl");
    assertReadSictusTerms(55, "query.pl");
    assertReadSictusTerms(119, "reducer.pl");
    assertReadSictusTerms(25, "sendmore.pl");
    assertReadSictusTerms(144, "simple_analyzer.pl");
    assertReadSictusTerms(5, "tak.pl");
    assertReadSictusTerms(64, "unify.pl");
    assertReadSictusTerms(8, "zebra.pl");
  }

  @Test
  void testParseSourceFiles() {
    assertReadTerms(17, "calc.p");
    assertReadTerms(23, "tictac.p");
    assertReadTerms(33, "poly.p");
    assertReadTerms(36, "cover.p", Op.make(400, FY, "private"), Op.make(400, XFX, "is_atom"));
    assertReadTerms(70, "sincos.p");
    assertReadTerms(49, "moddiv.p");
    assertReadTerms(48, "eqless.p");
    assertReadTerms(34, "bitwise.p");
    assertReadTerms(64, "basic.p");
    assertReadTerms(74, "signal.p");
    assertReadTerms(21, "pred.p");
    assertReadTerms(85, "logical.p");
    assertReadTerms(67, "kernel.p");
    assertReadTerms(20, "ConcurrentList.pl");
    assertReadTerms(5, "points_test.pl", Op.make(800, XFX, "<-"), Op.make(850, XFY, "returns"));
    assertReadTerms(6, "points_test2.pl", Op.make(800, XFX, "<-"), Op.make(850, XFY, "returns"));
    assertReadTerms(3, "Factorial.pl");
    assertReadTerms(9, "Mutex.pl");
    assertReadTerms(4, "PrivateQueue.pl");
    assertReadTerms(2, "PublicQueue.pl");
    assertReadTerms(17, "SequentialList.pl");
    assertReadTerms(26, "sec812.pro");
    assertReadTerms(25, "sec816.pro");
    assertReadTerms(32, "sec811.pro");
    assertReadTerms(8, "einstein_puzzle.pro");
    assertReadTerms(14, "simple.pl");
    assertReadTerms(8, "examp.pl");
    assertReadTerms(8, "alpha.pl", Op.SWI_CPL);
    assertReadTerms(35, "array.pl");
    assertReadTerms(12, "bdiag.pl", Op.GNU_FD);
    assertReadTerms(15, "bdonald.pl", Op.GNU_FD);
    assertReadTerms(89, "bridge1.pl", Op.GNU_FD);
    assertReadTerms(207, "scheduleevents.pl");
    assertReadTerms(7, "eliza.pl");
    assertReadTerms(6, "houses_puzzle.pl");
    assertReadTerms(18, "grammar.pl");
    assertReadTerms(6, "sudoku.pl", Op.SWI_CPL);
    assertReadTerms(6, "knights.pl", Op.SWI_CPL);
    assertReadTerms(39, "golog.pl",
        Op.make(800, XFY, "&"),
        Op.make(850, XFY, "v"),
        Op.make(870, XFY, "=>"),
        Op.make(880, XFY, "<=>"),
        Op.make(880, XFY, ":"),
        Op.make(960, XFY, "#")
    );
    assertReadTerms(3, "hanoi.pl");
    assertReadTerms(16, "likes.pl");
    assertReadTerms(7, "dmalloc.pl");
    assertReadTerms(2910, "moviedb.pl");
    assertReadTerms(24, "xref_packages.pl");
    assertReadTerms(6, "test_loop.pl");
    assertReadTerms(11, "linprog.pl");
    assertReadTerms(6, "queue_send.pl");
    assertReadTerms(8, "pooltest.pl");
    assertReadTerms(5, "test_eclipse.pl");
    assertReadTerms(17, "test_sandbox.pl");
    assertReadTerms(151, "test_arith.pl");
    assertReadTerms(168, "html_text.pl");
    assertReadTerms(87, "recursive.pl");
    assertReadTerms(10, "calltree.pl");
    assertReadTerms(53, "basics.pl");
    assertReadTerms(28, "analysis.pl", Op.make(500, OpAssoc.XFX, "@"));
    assertReadTerms(4, "sendmoney.pl", Op.SWI_CPL);
    assertReadTerms(75, "sictus.pl", Op.make(900, OpAssoc.XFX, "=>"), Op.make(800, XFY, "&"),
        Op.make(300, OpAssoc.XFX, ":"));
  }

  @Test
  void testOperatorLooksAsAtomAndPartOfAtom() {
    assertEquals("module", parseEd("module.").next().getText());
  }

  @Test
  void testStreamPositionForTerms() {
    PrologTerm atom = parseEd("\n     'hello'.").next();
    assertEquals(6, atom.getPos());
    assertEquals(2, atom.getLine());

    atom = parseEd("\n     12345.").next();
    assertEquals(6, atom.getPos());
    assertEquals(2, atom.getLine());

    atom = parseEd("\n     [1,2,3,4,5].").next();
    assertEquals(6, atom.getPos());
    assertEquals(2, atom.getLine());

    final PrologList list = (PrologList) parseEd("\n\n\n\n   [   [1,2],3].").next();
    assertEquals(4, list.getPos());
    assertEquals(5, list.getLine());

    assertEquals(8, list.getHead().getPos());
    assertEquals(5, list.getHead().getLine());

    final PrologStruct mainterm = (PrologStruct) parseEd(
        "  %\ncube (X,'hello',1000) :- \n Y is X * X * X.").next();

    // ':-'
    assertEquals(23, mainterm.getPos());
    assertEquals(2, mainterm.getLine());

    final PrologStruct leftPart = (PrologStruct) mainterm.getTermAt(0);
    // 'cube(X,Y)'
    assertEquals(1, leftPart.getPos());
    assertEquals(2, leftPart.getLine());
    // 'X'
    assertEquals(7, leftPart.getTermAt(0).getPos());
    assertEquals(2, leftPart.getTermAt(0).getLine());
    // 'hello'
    assertEquals(9, leftPart.getTermAt(1).getPos());
    assertEquals(2, leftPart.getTermAt(1).getLine());
    // 1000
    assertEquals(17, leftPart.getTermAt(2).getPos());
    assertEquals(2, leftPart.getTermAt(2).getLine());

    final PrologStruct rightPart = (PrologStruct) mainterm.getTermAt(1);
    // 'is'
    assertEquals(4, rightPart.getPos());
    assertEquals(3, rightPart.getLine());
    // 'Y'
    assertEquals(2, rightPart.getTermAt(0).getPos());
    assertEquals(3, rightPart.getTermAt(0).getLine());

    final PrologStruct structure =
        (PrologStruct) parseEd("test(\n1,  \n2, 4*2,  \n3,4,\n5,6*7,8).").next();
    // 4*2
    assertEquals(5, structure.getTermAt(2).getPos());
    assertEquals(3, structure.getTermAt(2).getLine());
    // 6*7
    assertEquals(4, structure.getTermAt(6).getPos());
    assertEquals(5, structure.getTermAt(6).getLine());
  }

  private void assertZFZOperatorStruct(final String operator, final PrologTerm term) {
    assertEquals(TermType.STRUCT, term.getType());
    final PrologStruct struct = (PrologStruct) term;
    assertEquals(2, struct.getArity());
    assertEquals(TermType.OPERATOR, struct.getFunctor().getType());
    assertEquals(operator, struct.getFunctor().getText());
  }

  @Test
  void testPairOperatorsWithoutWhitespaces() {
    final PrologParser parser = parseEd("a=..b.a..b.", of(FLAG_NONE, Op.SWI_CPL));
    assertZFZOperatorStruct("=..", parser.next());
    assertZFZOperatorStruct("..", parser.next());
    assertFalse(parser.hasNext());
  }

  @Test
  void testSingleOperatorAsAtom() {
    final PrologStruct structure = (PrologStruct) parseEd("not/stream.").next();
    assertZFZOperatorStruct("/", structure);
    assertEquals(ATOM, structure.getTermAt(0).getType());
    assertEquals("not", structure.getTermAt(0).getText());
    assertEquals(ATOM, structure.getTermAt(1).getType());
    assertEquals("stream", structure.getTermAt(1).getText());
  }

  @Test
  void testRecognizingUserOperatorsWhichSimilarMetaOperators() {
    final Map<String, OpContainer> operators = new HashMap<>();
    operators.put("(((", make(Op.make(1, OpAssoc.FX, "(((")));
    operators.put("...", make(Op.make(1200, XF, "...")));
    final StubContext stubContext = new StubContext(operators);

    final PrologStruct structure = (PrologStruct) parseEd("(((hello....", stubContext).next();
    assertEquals("...", structure.getFunctor().getText());
    assertEquals("(((", structure.getTermAt(0).getFunctor().getText());
    assertEquals("hello", ((PrologStruct) structure.getTermAt(0)).getTermAt(0).getText());
  }

  @Test
  void testOperatorNameAsAtomicFunctor() {
    final PrologStruct structure = (PrologStruct) parseEd("'mod'(_,_,_,_).").next();
    assertEquals("mod", structure.getFunctor().getText());
    assertNotSame(structure.getFunctor().getType(), TermType.OPERATOR);
    assertEquals(4, structure.getArity());
  }

  @Test
  void testOperatorNameAsFunctor_EmptyBrackets() {
    final PrologParserException ex =
        assertThrows(PrologParserException.class, () -> parseEd("+().").next());
    assertEquals(2, ex.getPos());
    assertEquals(1, ex.getLine());
  }

  @Test
  void testAtomAsFunctor_EmptyBrackets() {
    final PrologParserException ex =
        assertThrows(PrologParserException.class, () -> parseEd("'hello'().").next());
    assertEquals(8, ex.getPos());
    assertEquals(1, ex.getLine());
  }

  @Test
  void testAtomInSequenceWithNameSimilarOperator() {
    final PrologStruct structure = (PrologStruct) parseEd("functor(1,2,3,'mod',5,6).").next();
    assertEquals(6, structure.getArity());
    assertEquals("1", structure.getTermAt(0).getText());
    assertEquals("2", structure.getTermAt(1).getText());
    assertEquals("3", structure.getTermAt(2).getText());
    assertEquals("mod", structure.getTermAt(3).getText());
    assertEquals("5", structure.getTermAt(4).getText());
    assertEquals("6", structure.getTermAt(5).getText());
  }

  @Test
  void testHelloWorld() {
    final PrologStruct rule = (PrologStruct) parseEd("hello :- world,!.").next();
    final PrologStruct and = (PrologStruct) rule.getTermAt(1);
    assertEquals("hello world!", rule.getTermAt(0).getText() + ' ' + and.getTermAt(0).getText() +
        and.getTermAt(1).getText());
  }

  @Test
  void testParserStream() {
    PrologParser parser =
        parseEd("z(some).a(X):-[X].b('\\'hello world\\'').list([_|Tail]) :- list(Tail).");
    final String joined = parser.stream().map(PrologTerm::toString).collect(joining(". ", "", "."));
    parser = parseEd(joined);
    assertEquals(joined, parser.stream().map(PrologTerm::toString).collect(joining(". ", "", ".")));
    assertEquals("z(some). a(X) :- [X]. b('\\'hello world\\''). list([_|Tail]) :- list(Tail).",
        joined);
  }

  @Test
  void testStructStream() {
    PrologParser parser = parseEd("s(1,2,3,4,5,6,7,8).");
    assertEquals("1\n"
        + "2\n"
        + "3\n"
        + "4\n"
        + "5\n"
        + "6\n"
        + "7\n"
        + "8", parser.next().stream().map(PrologTerm::toString).collect(joining("\n")));
  }

  @Test
  void testListStream() {
    PrologParser parser = parseEd("[1,2,3,4,5,6,7,8|_].");
    assertEquals("1\n"
        + "2\n"
        + "3\n"
        + "4\n"
        + "5\n"
        + "6\n"
        + "7\n"
        + "8\n"
        + "_", parser.next().stream().map(PrologTerm::toString).collect(joining("\n")));
  }

  @Test
  void testTermStreamFlat() {
    PrologParser parser = parseEd("some(hello,world,[1,2,3|X],end).");
    final String joined = parser.stream()
        .flatMap(PrologTerm::stream)
        .flatMap(PrologTerm::stream)
        .map(PrologTerm::toString)
        .collect(joining("\n"));

    assertEquals("hello\n"
        + "world\n"
        + "1\n"
        + "2\n"
        + "3\n"
        + "X\n"
        + "end", joined);
  }

  private String parseSortAndJoin(final String text) {
    return parseEd(text).stream().sorted().map(PrologTerm::toString).collect(joining("\n"));
  }

  @Test
  void testStringWithIsoControl() {
    assertEquals("[97, 98, 99]", parseIso("[0'a,0'b,0'c].").next().toString());
    assertEquals("\u0007\b\f\n\r\t\u000B#\u0013\\'\"`", parseIso(
        "'\\a\\b\\f\\n\\r\\t\\v\\x23\\\\23\\\\\\\\'\\\"\\`'.").next().getText());
    assertThrows(PrologParserException.class, () -> parseEd("'hello\u0000world'.").next());
    assertEquals("'hello\\nworld'", parseEd("'hello\\\nworld'.").next().toString());
    assertEquals("'hello\\nworld'", parseEd("'hello\\\r\nworld'.").next().toString());
  }

  @Test
  void testSignedNumerics_EdingburghParser() {
    assertEquals(-1, ((PrologNumeric) parseEd("-1.").next()).getNumber().intValue());
    assertEquals(-1.1f, ((PrologNumeric) parseEd("-1.1.").next()).getNumber().floatValue(),
        Float.MIN_NORMAL);
    assertEquals(1, ((PrologNumeric) parseEd("1.").next()).getNumber().intValue());
    assertEquals(1.1f, ((PrologNumeric) parseEd("1.1.").next()).getNumber().floatValue(),
        Float.MIN_NORMAL);
  }

  @Test
  void testSignedNumerics_GenericParser() {
    assertThrows(PrologParserException.class, () -> parseGen("-1.").next());
    assertThrows(PrologParserException.class, () -> parseGen("+1.").next());
    assertThrows(PrologParserException.class, () -> parseGen("-1.1.").next());
    assertThrows(PrologParserException.class, () -> parseGen("+1.1.").next());

    assertEquals(1, ((PrologNumeric) parseGen("1.").next()).getNumber().intValue());
    assertEquals(1.1f, ((PrologNumeric) parseGen("1.1.").next()).getNumber().floatValue(),
        Float.MIN_NORMAL);

    assertEquals(-1,
        ((PrologNumeric) parseGen("-1.", of(FLAG_NONE, Op.ISO_UNARY_MINUS)).next()).getNumber()
            .intValue());
    assertEquals("+ 1", parseGen("+1.", of(FLAG_NONE, Op.GNU_UNARY_PLUS)).next().toString());
    assertEquals("+ 1.1", parseGen("+1.1.", of(FLAG_NONE, Op.GNU_UNARY_PLUS)).next().toString());
    assertEquals(-1.1f,
        ((PrologNumeric) parseGen("-1.1.", of(FLAG_NONE, Op.ISO_UNARY_MINUS)).next()).getNumber()
            .floatValue(), Float.MIN_NORMAL);
  }

  @Test
  void testMisc() {
    assertNotNull(parseEd("''(123_345)**[].").next());
    assertEquals("'+'", parseEd("+.").next().toString());
    assertEquals("some(a, '+', b)", parseEd("some(a,+,b).").next().toString());
    assertEquals("some('+', '+', '+')", parseEd("some(+,+,+).").next().toString());
  }

  @Test
  void testNonCompletedClause() {
    assertThrows(PrologParserException.class, () -> parseEd("a").next());
    assertThrows(PrologParserException.class, () -> parseEd("123").next());
    assertThrows(PrologParserException.class, () -> parseEd("\"dsdsd\"").next());
    assertThrows(PrologParserException.class, () -> parseEd("some(1,2)").next());
    assertThrows(PrologParserException.class, () -> parseEd("[1,2,3,4]").next());
    assertThrows(PrologParserException.class, () -> parseEd("[1,2,3").next());
    assertThrows(PrologParserException.class, () -> parseEd("a(").next());
    assertThrows(PrologParserException.class, () -> parseEd("1.22").next());
  }

  @Test
  void testOperatorAsFunctor() {
    assertOperatorAsFunctor("1 + 2", parseEd("+(1,2).").next());
    assertOperatorAsFunctor("+ 1", parseEd("+(1).").next());
    assertOperatorAsFunctor("1 + 2 + 3", parseEd("+(1,2)+3.").next());
    assertOperatorAsFunctor("1 + (2 + 3)", parseEd("1++(2,3).").next());
    assertOperatorAsFunctor("1 + 2 + 34", parseEd("+(1,2)+34.").next());
    assertOperatorAsFunctor("1 / 2 / 4 / (8 / 3)", parseEd("/(1,2)/4/(8/3).").next());
    assertOperatorAsFunctor("1 / 2 / 4 / (8 , 3)", parseEd("/(1,2)/4/(8,3).").next());
    assertOperatorAsFunctor("1 : 2", parseEd(":(1,2).").next());
    assertOperatorAsFunctor("2 + 3 * 4 =:= X", parseIso("=:=(+(2,*(3,4)),X).").next());
  }

  @Test
  void testPairOfOperatorsWithIncompatiblePrecedence() {
    assertEquals("- ((discontiguous))", parseEd("-discontiguous.").next().toString());
    assertEquals("aab",
        parseEd("aab.", DefaultParserContext.of(FLAG_NONE, Op.make(400, XF, "aabc"))).next()
            .toString());
    assertEquals("1 - - -1", parseEd("1---1.").next().toString());
    assertEquals("1 + 1 * a * a + a - 1", parseEd("1+1*a*a+a-1.").next().toString());
    assertEquals("-1 + 2 ** (- 3 ** (-4))", parseEd("-1+2**-3**-4.").next().toString());
    assertEquals("X = (discontiguous)", parseEd("X=discontiguous.").next().toString());
    assertEquals("2 ** (-1)", parseEd("2**-1.").next().toString());
    assertEquals("0.2 is 5 ** (-1)", parseEd("0.2 is 5** -1.").next().toString());
    assertEquals("a : b :> c :> d", parseEd("a:b:>c:>d.",
        DefaultParserContext.of(ParserContext.FLAG_NONE, Op.make(500, XFY, ":>"))).next()
        .toString());
    assertEquals("X = (a , b , c ; (dynamic d))",
        parseEd("X=(a,b,c; dynamic d).").next().toString());
    assertThrows(PrologParserException.class, () -> parseEd("a :- b :- c.").next());
    assertThrows(PrologParserException.class, () -> parseEd("?-mother(pam,bob);").next());
  }

  @Test
  void testOperatorAsFunctorWithUnsupportedArity() {
    assertEquals("':'(1, 2, 3)", parseEd(":(1,2,3).").next().toString());
    assertEquals("+ (1 , 2 , 3)", parseEd("+(1,2,3).").next().toString());
    assertEquals("':'(1, (2 , 3), 4)", parseEd(":(1,(2,3),4).").next().toString());
    assertEquals("':'(1)", parseEd(":(1).").next().toString());
  }

  @Test
  void testBlockWithOperators() {
    assertOperatorAsFunctor("+some.", "+", FY, 1, "+ some");
    assertOperatorAsFunctor("+(!,fail).", "+", YFX, 2, "'!' + fail");
    assertOperatorAsFunctor("+((!,fail)).", "+", FY, 1, "+ (('!' , fail))");
    assertOperatorAsFunctor("+(((!,fail))).", "+", FY, 1, "+ (('!' , fail))");
  }

  @Test
  void testBlock() {
    assertEquals("(1)", parseEd("(1).").next().toString());
    assertEquals("(1 , 2)", parseEd("(1,2).").next().toString());
    assertEquals("(1 - 2)", parseEd("(1-2).").next().toString());
    assertEquals("(1 - 2 * (34 / 33))", parseEd("(1-2*(34/33)).").next().toString());
    assertEquals("(1 , 2 , 3)", parseEd("(1,2,3).").next().toString());
    assertEquals("((1 , 2) , 3)", parseEd("((1,2),3).").next().toString());
    assertEquals("((1 , 2) , 3)", parseEd("(((1,2),3)).").next().toString());
    assertEquals("[A|(B | C)]", parseEd("[A|((((B|C))))].").next().toString());
    assertEquals("vertical(line(point(X, Y), point(X, Z)))",
        parseEd("vertical(line(point(X,Y), point(X,Z))).").next().toString());
    assertEquals(
        "move(state(middle, onbox, middle, hasnot), grasp, state(middle, onbox, middle, has))",
        parseEd(
            "move(state(middle, onbox, middle, hasnot),grasp,state(middle, onbox, middle, has)).").next()
            .toString());
    assertEquals("X1 =< X2 , X2 =< (X1 + LENGTH) , (Y1 - LENGTH) =< Y2 , Y2 =< Y1",
        parseEd("X1 =< X2, X2 =< (X1 + LENGTH), (Y1-LENGTH) =< Y2, Y2 =< Y1.").next().toString());
  }

  @Test
  void testAloneOperatorAsAtom() {
    assertEquals(TermType.ATOM, parseEd("/.").next().getType());
    assertEquals(TermType.ATOM, parseEd("-.").next().getType());
    assertEquals(TermType.ATOM, parseEd("+.").next().getType());
  }

  @Test
  void testUnexpectedlyEndedReadStream() {
    final Random rnd = new Random(12345);

    final AtomicInteger completedClauseCounter = new AtomicInteger();

    final int ATTEMPTS = 100;

    for (int i = 0; i < ATTEMPTS; i++) {
      final int numChars = rnd.nextInt(5) + i * 3;
      assertThrows(PrologParserException.class, () -> {
        final PrologParser parser = parseEd(
            new InputStreamReader(
                new PrologSourceKoi7Generator(rnd.nextBoolean(),
                    numChars,
                    false), StandardCharsets.UTF_8), new DefaultParserContext(FLAG_BLOCK_COMMENTS));

        while (parser.hasNext()) {
          assertNotNull(parser.next());
        }
        completedClauseCounter.incrementAndGet();
        throw new PrologParserException("Whole clause has been read", -1, -1);
      });
    }

    assertTrue(completedClauseCounter.get() < Math.round(0.1 * ATTEMPTS));
  }

  @Test
  void testNonLatinChars() {
    assertEquals("Ы is П + Ш", parseEd("Ы is П + Ш.").next().toString());
    assertEquals("Привет123", parseEd("Привет123.").next().toString());
    assertThrows(PrologParserException.class, () -> parseEd("Привет123(a).").next().toString());
    assertEquals("привет123", parseEd("привет123.").next().toString());
    assertEquals("привет", parseEd("привет.").next().toString());
  }

  @Test
  void testVarAsFunctor() {
    assertThrows(PrologParserException.class,
        () -> new GenericPrologParser(new StringReader("X(a)."),
            DefaultParserContext.of(FLAG_NONE)).next());

    final PrologStruct struct = (PrologStruct) new GenericPrologParser(new StringReader("X(a)."),
        DefaultParserContext.of(FLAG_VAR_AS_FUNCTOR)).next();
    final PrologVar functor = (PrologVar) struct.getFunctor();
    assertEquals("X", functor.getText());
    assertEquals(1, struct.getArity());
    assertEquals("X(a)", struct.toString());

    final PrologStruct structB =
        (PrologStruct) new GenericPrologParser(new StringReader("X(a),Y(b),_Z(d)."),
            DefaultParserContext.of(FLAG_VAR_AS_FUNCTOR)).next();
    assertEquals(structB.getFunctor(), Op.METAOPERATOR_COMMA);
    assertEquals(TermType.VAR, structB.getTermAt(0).getFunctor().getType());
    assertEquals("X", structB.getTermAt(0).getFunctor().getText());

    final PrologStruct structC = (PrologStruct) structB.getTermAt(1);
    assertEquals(TermType.VAR, structC.getTermAt(0).getFunctor().getType());
    assertEquals("Y", structC.getTermAt(0).getFunctor().getText());
    assertEquals(TermType.VAR, structC.getTermAt(1).getFunctor().getType());
    assertEquals("_Z", structC.getTermAt(1).getFunctor().getText());
    assertEquals("X(a) , Y(b) , _Z(d)", structB.toString());
  }

  @Test
  void testAllowZeroStruct() {
    assertThrows(PrologParserException.class,
        () -> new GenericPrologParser(new StringReader("a()."),
            DefaultParserContext.of(FLAG_NONE)).next());
    final PrologStruct struct = (PrologStruct) new GenericPrologParser(new StringReader("a()."),
        DefaultParserContext.of(FLAG_ZERO_STRUCT)).next();
    final PrologAtom functor = (PrologAtom) struct.getFunctor();
    assertEquals("a", functor.getText());
    assertEquals(0, struct.getArity());
    assertEquals("a()", struct.toString());

    final PrologStruct structB =
        (PrologStruct) new GenericPrologParser(new StringReader("a(/*some comment*/)."),
            DefaultParserContext.of(FLAG_ZERO_STRUCT | FLAG_BLOCK_COMMENTS)).next();
    assertEquals("a()", structB.toString());

    final PrologStruct structC =
        (PrologStruct) new GenericPrologParser(new StringReader("a(),b(),c()."),
            DefaultParserContext.of(FLAG_ZERO_STRUCT | FLAG_BLOCK_COMMENTS)).next();
    assertEquals("a() , b() , c()", structC.toString());

    final PrologStruct structD =
        (PrologStruct) new GenericPrologParser(new StringReader("A(),X(),Z()."),
            DefaultParserContext.of(
                FLAG_VAR_AS_FUNCTOR | FLAG_ZERO_STRUCT | FLAG_BLOCK_COMMENTS)).next();
    assertEquals("A() , X() , Z()", structD.toString());

  }

  @Test
  void testStandardTermOrder() {
    assertEquals("1\n2\n3\n4\n5", parseSortAndJoin("5. 3. 1. 2. 4."));
    assertEquals("1.3\n2\n3.6\n4\n5.23", parseSortAndJoin("5.23. 3.6. 1.3. 2. 4."));
    assertEquals("1.3\n2\n3.6\n4\n5.23", parseSortAndJoin("5.23.3.6.1.3.2. 4."));
    assertEquals("a\nb\nc\nd\ne", parseSortAndJoin("c.e.b.a.d."));
    assertEquals("[]\n[1, 2]\n[1, 2, 3]\n[5, 6, 7]\n[8]",
        parseSortAndJoin("[1,2,3].[1,2].[].[5,6,7].[8]."));
    assertEquals("u(8)\nl(1, 2)\ng(5, 6, 7)\nh(1, 2, 3)",
        parseSortAndJoin("h(1,2,3).l(1,2).g(5,6,7).u(8)."));
    assertEquals("X\nY\n_\n34.112\n112\natm\n[]\nm(34)\n[1, 2, 3]\na(1, 2)",
        parseSortAndJoin("a(1,2).[1,2,3].X.112.Y.34.112. atm. [].m(34)._."));
  }

  @Test
  void testBlockAsOnlyArgument() {
    final PrologStruct parsed = (PrologStruct) parseEd("test((1,2)).").next();
    assertEquals("test", parsed.getFunctor().getText());
    assertEquals(1, parsed.getArity());
    final PrologStruct arg = (PrologStruct) parsed.getTermAt(0);
    assertTrue(arg.isAnyBlock());
    assertFalse(arg.isCurlyBlock());
    assertEquals("()", arg.getFunctor().getText());
    assertEquals(1, arg.getFunctor().getArity());
    assertEquals("1", ((PrologStruct) arg.getTermAt(0)).getTermAt(0).getText());
    assertEquals("2", ((PrologStruct) arg.getTermAt(0)).getTermAt(1).getText());
  }

  @Test
  void testCurlyBlock() {
    assertEquals("{a , b , c} = Syntax", parseEd("{a,b,c} = Syntax.").next().toString());
    assertEquals("{{a , b , c}} = Syntax", parseEd("{{a,b,c}} = Syntax.").next().toString());
    assertEquals("{{{a , b , c}}} = Syntax", parseEd("{{{a,b,c}}} = Syntax.").next().toString());
    assertEquals("{{(a , b , c)}} = Syntax", parseEd("{{(a,b,c)}} = Syntax.").next().toString());
    assertEquals("({(a , b , c)}) = Syntax", parseEd("({(a,b,c)}) = Syntax.").next().toString());
    assertEquals("{({a , b , c})} = Syntax", parseEd("{({a,b,c})} = Syntax.").next().toString());
    assertEquals("{(a , b , c)} = Syntax", parseEd("{((a,b,c))} = Syntax.").next().toString());
  }

  @Test
  void testCurlyBlockAsOnlyArgument() {
    final PrologStruct parsed = (PrologStruct) parseEd("test({1,2}).").next();
    assertEquals("test", parsed.getFunctor().getText());
    assertEquals(1, parsed.getArity());
    final PrologStruct arg = (PrologStruct) parsed.getTermAt(0);
    assertTrue(arg.isAnyBlock());
    assertTrue(arg.isCurlyBlock());
    assertEquals("{}", arg.getFunctor().getText());
    assertEquals(1, arg.getFunctor().getArity());
    assertEquals("1", ((PrologStruct) arg.getTermAt(0)).getTermAt(0).getText());
    assertEquals("2", ((PrologStruct) arg.getTermAt(0)).getTermAt(1).getText());
  }

  /**
   * Based on cases represented on
   * <a href="https://www.complang.tuwien.ac.at/ulrich/iso-prolog/conformity_testing">the
   * page</a>
   */
  @Test
  void testConformity() {
    assertThrows(PrologParserException.class, () -> parseEd("{:- :- c} = {:-(:-,c)}.").next());
    assertThrows(PrologParserException.class, () -> parseEd("{1} = {}(1).").next());

    assertEquals("writeq({- = xf1})", parseEd("writeq({- =xf1}).").next().toString());
    assertEquals("X = {,}", parseEd("X ={,}.").next().toString());
    assertEquals("{- = -1} = {(- =) - 1}", parseEd("{- = - 1}={(-(=)) - 1}.").next().toString());
    assertEquals("{- - c} = {- - c}", parseEd("{- - c}={-(-(c))}.").next().toString());

    assertEquals("writeq([+ {a}, + []])", parseEd("writeq([+{a},+[]]).").next().toString());
    assertEquals("writeq(- {a})", parseEd("writeq(-{a}).").next().toString());
    assertEquals("writeq(- {})", parseEd("writeq(-{}).").next().toString());

    assertEquals("writeq([a, b|','])", parseEd("writeq([a,b|',']).").next().toString());
    assertEquals("X = 1", parseEd("X = 2'1.").next().toString());
    assertEquals("writeq(- - 1)", parseEd("writeq(-(-(1))).").next().toString());
    assertEquals("writeq(- - a)", parseEd("writeq(-(-a)).").next().toString());
    assertEquals("writeq(- - - a)", parseEd("writeq(-(-(-a))).").next().toString());
    assertEquals("writeq(- p(c))", parseEd("writeq(-p(c)).").next().toString());
    assertEquals("writeq(- ['-'])", parseEd("writeq(-[-]).").next().toString());
    assertEquals("writeq(- '-')", parseEd("writeq(-(-)).").next().toString());
    assertEquals("writeq(- a)", parseEd("writeq(-a).").next().toString());
    assertEquals("writeq(- ((a , b)))", parseEd("writeq(-((a,b))).").next().toString());
    assertEquals("writeq(- a ^ 2)", parseEd("writeq(-(a^2)).").next().toString());
    assertEquals("writeq(- 1 ^ 2)", parseEd("writeq(-(1^2)).").next().toString());
    assertEquals("writeq(- -1)", parseEd("writeq(-(-1)).").next().toString());
    assertEquals("writeq('\\e')", parseEd("writeq('\\033\\').").next().toString());

    assertEquals("writeq('a\\n b')",
        parseEd("writeq('a\\\n b'). % \"a\\\\\\n b\" ").next().toString());
    assertEquals("writeq('a\\nb')",
        parseEd("writeq('a\\\nb'). % \"a\\\\\\nb\" ").next().toString());
    assertEquals("writeq('\\n')", parseEd("writeq('\\\n'). % \"\\\\ \\n\"").next().toString());
    assertEquals("writeq('a\\n b')",
        parseEd("writeq('a\\\n b'). % \"a\\\\\\n b\"").next().toString());
    assertEquals("writeq('a\\nb')",
        parseEd("writeq('a\\\nb'). % \"a\\\\\\nb\" ").next().toString());
    assertEquals("writeq('\\na')", parseEd("writeq('\\\na'). % \"\\\\\\na\" ").next().toString());
    assertEquals("writeq('\\n')", parseEd("writeq('\\\n'). % \"\\\\\\n\" ").next().toString());
    assertEquals("* = *", parseEd("* = * .").next().toString());
    assertEquals("[:- - c] = [(:- - c)]", parseEd("[:- -c] = [(:- -c)].").next().toString());
    assertEquals("X = '\\'", parseEd("X = '\\\\' .").next().toString());
    assertEquals("X = `a`", parseEd("X = `a`.").next().toString());
    assertEquals("writeq(- (a * b))", parseEd("writeq(- (a*b)).").next().toString());
    assertEquals("writeq(\\ (a * b))", parseEd("writeq(\\ (a*b)).").next().toString());
    assertEquals("writeq([1])", parseEd("/**/ writeq([1]).").next().toString());
    assertEquals("writeq(- [1])", parseEd(" /**/ writeq(-[1]).").next().toString());
    assertEquals("write_canonical(1 p (p p 2))", parseEd("write_canonical(1 p p p 2).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS, Op.make(9, FY, "p"),
            Op.make(9, YFX, "p"))).next().toString());
    assertEquals("write_canonical(1 p p p 2)", parseEd("write_canonical(1 p p p 2).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS, Op.make(9, FY, "p"),
            Op.make(9, XFY, "p"))).next().toString());
    assertEquals("write_canonical(1 p p p 2)", parseEd("write_canonical(1 p p p 2).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS, Op.make(7, FY, "p"),
            Op.make(9, YFX, "p"))).next().toString());
    assertEquals("atom('.\\'-\\'.')", parseEd("atom('.\\'-\\'.').").next().toString());
    assertEquals("op(0, xfy, '|')", parseEd("op(0,xfy,'|').").next().toString());
    assertEquals("writeq((a | b))", parseEd("/**/ writeq((a|b)).").next().toString());
    assertEquals("X is 10.0 ** (-323)", parseEd("X is 10.0** -323.").next().toString());
    assertEquals("10E-324.0 =:= 10.0 ** (-323)",
        parseEd("1.0e-323=:=10.0** -323.").next().toString());
    assertEquals("-1 = -1", parseEd("-1 = -0x1.").next().toString());
    assertEquals("T = t(1, 1, 1)", parseEd("T = t(0b1,0o1,0x1).").next().toString());
    assertEquals("X is 1 mod 2", parseEd("X is 0b1mod 2.").next().toString());
    assertEquals("writeq((a --> b , c | d))",
        parseEd("/**/ writeq((a-->b,c|d)).").next().toString());
    assertEquals("writeq([(a | b)])", parseEd("/**/ writeq([(a|b)]).").next().toString());
    assertEquals("X = 7", parseEd("X/* /*/=7.").next().toString());
    assertEquals("X = 7", parseEd("X/*/*/=7.").next().toString());
    assertEquals("atom(- $)", parseEd("atom(-$).").next().toString());
    assertEquals("\\", parseEd("\\ .").next().toString());
    assertEquals("write_canonical(_ + _)", parseEd("write_canonical(_+_).").next().toString());
    assertEquals("writeq(nop(1))", parseEd("writeq(nop (1)).").next().toString());
    assertEquals("writeq(1 = f)", parseEd("/**/ writeq(1 = f).").next().toString());
    assertEquals("write_canonical(a - - - b)",
        parseEd("write_canonical(a- - -b).").next().toString());
    assertEquals("writeq(0 bop 2)", parseEd("writeq(0bop 2).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS,
            Op.make(9, YFX, "bop", "bo", "b", "op", "xor"))).next().toString());
    assertEquals("writeq(0 bop 2)", parseEd("/**/ writeq(0 bop 2).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS,
            Op.make(9, YFX, "bop", "bo", "b", "op", "xor"))).next().toString());
    assertEquals("writeq(0 bo 2)", parseEd("/**/ writeq(0bo 2).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS,
            Op.make(9, YFX, "bop", "bo", "b", "op", "xor"))).next().toString());
    assertEquals("writeq(0 b 2)", parseEd("/**/ writeq(0b 2).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS,
            Op.make(9, YFX, "bop", "bo", "b", "op", "xor"))).next().toString());
    assertEquals("writeq(0 op 2)", parseEd("/**/ writeq(0op 2).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS,
            Op.make(9, YFX, "bop", "bo", "b", "op", "xor"))).next().toString());
    assertEquals("writeq(0 xor 2)", parseEd("/**/ writeq(0xor 2).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS,
            Op.make(9, YFX, "bop", "bo", "b", "op", "xor"))).next().toString());
    assertEquals("writeq('^`')", parseEd("writeq('^`').").next().toString());
    assertEquals("writeq('\\b\\r\\f\\t\\n')",
        parseEd("writeq('\\b\\r\\f\\t\\n').").next().toString());
    assertEquals("writeq(-- a)", parseEd("writeq(--(a)).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS, Op.make(20, FX, "--"))).next().toString());
    assertEquals("writeq(-- a)", parseEd("writeq(--(a)).",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS, Op.make(0, FY, "--"))).next().toString());
    assertEquals("writeq(10 mod 2)", parseEd("writeq(0xamod 2).").next().toString());

    assertThrows(PrologParserException.class, () -> parseEd("integer(0'').").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq('\\^J').").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq(00'a).").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq(00'+'1).").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq(0B1).").next());
    assertThrows(PrologParserException.class, () -> parseEd("**/ writeq(0b2).").next());
    assertThrows(PrologParserException.class, () -> parseEd("/**/ writeq(0o8).").next());
    assertThrows(PrologParserException.class, () -> parseEd("/**/ write_canonical(a>).").next());
    assertThrows(PrologParserException.class,
        () -> parseEd("/**/ write_canonical((a>,b)).").next());
    assertThrows(PrologParserException.class, () -> parseEd("/**/ write_canonical(a> =b).").next());
    assertThrows(PrologParserException.class, () -> parseEd("/**/ write_canonical(a> >b).").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq(0'\\ ).").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq(0'\\c).").next());
    assertThrows(PrologParserException.class, () -> parseEd("char_code('\\^',X).").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq(0'\\z).").next());
    assertThrows(PrologParserException.class, () -> parseEd("atom($-).").next());
    assertThrows(PrologParserException.class, () -> parseEd("/**/ writeq(\"\\z\")").next());
    assertThrows(PrologParserException.class, () -> parseEd("/**/ write_canonical(fy yf).").next());
    assertThrows(PrologParserException.class, () -> parseEd("/**/ write_canonical(f f).").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq(0'f').",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS, Op.make(100, XF, "f"))).next());
    assertThrows(PrologParserException.class, () -> parseEd("/**/ X = 1.e.",
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS, Op.make(100, XF, "f"))).next());
    assertThrows(PrologParserException.class, () -> parseEd("/**/ writeq(1 .2).").next());
    assertThrows(PrologParserException.class, () -> parseEd("is 0'mod'1.").next());
    assertThrows(PrologParserException.class, () -> parseEd("X = '\\77777777777\\'.").next());
    assertThrows(PrologParserException.class, () -> parseEd("X = '\\N'.").next());
    assertThrows(PrologParserException.class, () -> parseEd("X = '\\9'.").next());
    assertThrows(PrologParserException.class, () -> parseEd("a = '\\141'.").next());
    assertThrows(PrologParserException.class, () -> parseEd("X = [] (1).").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq([a,b|,]).").next());
    assertThrows(PrologParserException.class, () -> parseEd("(- - - -) = -(-(-(-))).").next());
    assertThrows(PrologParserException.class, () -> parseEd("(- - -) = -(-(-)).").next());
    assertThrows(PrologParserException.class, () -> parseEd("(- -) = -(-).").next());
    assertThrows(PrologParserException.class, () -> parseEd(":- = :- .").next());
    assertThrows(PrologParserException.class, () -> parseEd("- = - .").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq(//*.*/).").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq(//*).").next());
    assertThrows(PrologParserException.class, () -> parseEd("X = 0'\\. .").next());
    assertThrows(PrologParserException.class, () -> parseEd("X = 0'\\.").next());
    assertThrows(PrologParserException.class, () -> parseEd("'\\\n''.").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq(.").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq('").next());
    assertThrows(PrologParserException.class, () -> parseEd("X = 0'\\u1.").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq('\\u1').").next());
    assertThrows(PrologParserException.class,
        () -> parseEd("writeq('\\\t'). % \"\\\\\\t\"").next());
    assertThrows(PrologParserException.class,
        () -> parseEd("writeq('\\ \n'). % \"\\\\ \\n\"").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq('\\ ').").next());
    assertThrows(PrologParserException.class, () -> parseEd("0'\\t=0' . % horiz. tab").next());
    assertThrows(PrologParserException.class, () -> parseEd("'").next());
    assertThrows(PrologParserException.class, () -> parseEd(")").next());
    assertThrows(NoSuchElementException.class, () -> parseEd(".").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq('\t'). % horiz. tab ").next());
    assertThrows(PrologParserException.class, () -> parseEd("writeq('\n').").next());
    assertThrows(PrologParserException.class, () -> parseEd("Finis ().").next());

    assertEquals("writeq('*/')", parseEd("writeq('*/').").next().toString());
    assertEquals("writeq('/**')", parseEd("writeq('/**').").next().toString());
    assertEquals("writeq('\\n')", parseEd("writeq('\\n').").next().toString());
    assertThrows(PrologParserException.class, () -> parseEd("writeq('\\9\\').").next());
    assertEquals("writeq('\\0\\')", parseEd("writeq('\\0\\').").next().toString());
    assertEquals("writeq('\\a')", parseEd("writeq('\\7\\').").next().toString());
    assertEquals("writeq(a * (b + c))", parseEd("writeq(a*(b+c)).").next().toString());
    assertEquals("writeq(f(;, '|', ';;'))", parseEd("writeq(f(;,'|',';;')).").next().toString());
    assertEquals("writeq('a\\n b')",
        parseEd("writeq('a\\\n b'). % \"a\\\\\\n b\"").next().toString());
    assertEquals("writeq(('-') - ('-'))", parseEd("writeq((-)-(-)).").next().toString());
    assertEquals("writeq(((':-') :- (':-')))", parseEd("writeq(((:-):-(:-))).").next().toString());
    assertEquals("writeq((*) = (*))", parseEd("writeq((*)=(*)).").next().toString());
    assertEquals("writeq([':-', '-'])", parseEd("writeq([:-,-]).").next().toString());
    assertEquals("writeq('\\b\\r\\f\\t\\n')",
        parseEd("writeq('\\b\\r\\f\\t\\n').").next().toString());
    assertEquals("writeq('^`')", parseEd("writeq('^`').").next().toString());
  }

  @Test
  void testWhiteSpaceInZeroQuotation() {
    assertThrows(PrologParserException.class,
        () -> parseEd("0' .", ParserContext.FLAG_ZERO_QUOTATION_CHARCODE).next());
    assertEquals("32", parseEd("0' .", ParserContext.FLAG_ZERO_QUOTATION_CHARCODE |
        ParserContext.FLAG_ZERO_QUOTATION_ALLOWS_WHITESPACE_CHAR).next().toString());
  }

}
