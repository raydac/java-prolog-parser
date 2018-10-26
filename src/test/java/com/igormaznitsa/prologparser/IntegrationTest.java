package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloat;
import com.igormaznitsa.prologparser.terms.PrologInteger;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologNumeric;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.PrologVariable;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import com.igormaznitsa.prologparser.tokenizer.PrologParser;
import com.igormaznitsa.prologparser.utils.StringUtils;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static com.igormaznitsa.prologparser.DefaultParserContext.of;
import static com.igormaznitsa.prologparser.ParserContext.*;
import static com.igormaznitsa.prologparser.terms.OpContainer.make;
import static com.igormaznitsa.prologparser.terms.PrologTerm.QuotingType.*;
import static com.igormaznitsa.prologparser.terms.TermType.ATOM;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFX;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFY;
import static java.util.stream.Collectors.joining;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class IntegrationTest {

  private static PrologParser parseCpl(final String str) {
    return new GenericPrologParser(new StringReader(str), DefaultParserContext.of(FLAG_BLOCK_COMMENTS | FLAG_ZERO_SINGLE_QUOTATION_CHAR_CODE, Op.join(Op.SWI, Op.SWI_CPL)));
  }

  private static PrologParser parseIso(final String str) {
    return new GenericPrologParser(new StringReader(str), DefaultParserContext.of(FLAG_NONE | FLAG_ZERO_SINGLE_QUOTATION_CHAR_CODE, Op.ISO));
  }

  private static PrologParser parseEd(final String str) {
    final ParserContext parserContext = mock(ParserContext.class);
    when(parserContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    when(parserContext.getFlags()).thenReturn(ParserContext.FLAG_BLOCK_COMMENTS | FLAG_ZERO_SINGLE_QUOTATION_CHAR_CODE);
    return parseEd(str, parserContext);
  }

  private static PrologParser parseEd(final String str, final ParserContext context) {
    return parseEd(new StringReader(str), context);
  }

  private static PrologParser parseEd(final Reader reader, final ParserContext context) {
    return new GenericPrologParser(reader, ParserContextChain.of(
        DefaultParserContext.of(FLAG_BLOCK_COMMENTS | FLAG_ZERO_SINGLE_QUOTATION_CHAR_CODE, Op.SWI), context));
  }

  private static PrologParser parseGen(final String str, final ParserContext context) {
    final ParserContext parserContext = mock(ParserContext.class);
    when(parserContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    when(parserContext.getFlags()).thenReturn(ParserContext.FLAG_BLOCK_COMMENTS | FLAG_ZERO_SINGLE_QUOTATION_CHAR_CODE);
    return new GenericPrologParser(new StringReader(str), ParserContextChain.of(context, parserContext));
  }

  private static GenericPrologParser parseGen(final String str) {
    final ParserContext parserContext = mock(ParserContext.class);
    when(parserContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    when(parserContext.getFlags()).thenReturn(ParserContext.FLAG_BLOCK_COMMENTS | FLAG_ZERO_SINGLE_QUOTATION_CHAR_CODE);
    return new GenericPrologParser(new StringReader(str), parserContext);
  }

  @Test
  public void testSwiCpl() {
    assertEquals("X in 5 .. 10 , 5 , Y #=< X + -1 , 6 , Y in 4 .. 8", parseCpl("X in 5..10,5,Y#=<X+ -1,6,Y in 4..8.").next().toString());
    assertEquals("#\\ X", parseCpl("#\\X.").next().toString());
    assertEquals("M #\\= 0 , S #\\= 0", parseCpl("M #\\= 0, S #\\= 0.").next().toString());
    assertEquals("Temp #< 800 , #\\ Startup #==> Temp #> 300", parseCpl("Temp#<800,#\\Startup#==>Temp#>300.").next().toString());
    assertEquals("A in 1 \\/ 3 \\/ 5 \\/ 8 #==> B #=< 8 , A in 9 .. 11 \\/ 14 #==> B #> 8", parseCpl("A in 1\\/3\\/5\\/8#==>B#=<8,A in 9..11\\/14#==>B#>8.").next().toString());
    assertEquals("[X, Y] ins 1 .. 3 , labeling([max(X), min(Y)], [X, Y])", parseCpl("[X,Y] ins 1..3,labeling([max(X),min(Y)],[X,Y]).").next().toString());
    assertEquals("sort_of_near_to(X, Y, N, M) :- X #> Y #==> X - N #>= Y #/\\ X - M - 1 #< Y , X #=< Y #==> Y - N #>= X #/\\ Y - M - 1 #< X", parseCpl("sort_of_near_to(X, Y, N, M):-X#>Y#==>X-N#>=Y#/\\X-M-1#<Y,X#=<Y#==>Y-N#>=X#/\\Y-M-1#<X.").next().toString());
    assertEquals("V in inf .. -2 \\/ 0 \\/ 2 .. 16 \\/ 18 .. sup", parseCpl("V in inf.. -2 \\/ 0 \\/ 2..16 \\/ 18..sup.").next().toString());
    assertEquals("X #> Y #==> X - N #>= Y #/\\ X - M - 1 #< Y , X #=< Y #==> Y - N #>= X #/\\ Y - M - 1 #< X", parseCpl("X#>Y#==>X-N#>=Y#/\\X-M-1#<Y,X #=< Y #==> Y - N #>= X #/\\ Y - M - 1 #< X.").next().toString());
    assertEquals("clpfd : run_propagator(even(X), MState) :- (integer(X) -> clpfd : kill(MState) , 0 is X mod 2 ; true)", parseCpl("clpfd:run_propagator(even(X),MState):-(integer(X)->clpfd:kill(MState),0is X mod 2;true).").next().toString());
    assertThrows(PrologParserException.class, () -> parseCpl("#\\X..#\\Y.").next());
  }

  @Test
  public void testParseStringWithSpecialChars() {
    final PrologParser parser = parseEd("'0\\'a \u0008Hello\\\nWorld\u0021\\r'.\"0'a \\xFF\\Another String\u0007\".");
    PrologTerm term = parser.next();

    assertEquals(ATOM, term.getTermType());
    assertEquals(PrologTerm.QuotingType.SINGLE_QUOTED, term.getQuotingType());
    assertEquals("0\\'a \\bHello\\nWorld!\\r", StringUtils.escapeString(term.getTermText(), SINGLE_QUOTED));

    term = parser.next();
    assertNotNull(term);
    assertEquals(PrologTerm.QuotingType.DOUBLE_QUOTED, term.getQuotingType());
    assertEquals("0'a \u00FFAnother String\u0007", term.getTermText());

  }

  private void assertPrologInteger(final long expected, final String parse) {
    final PrologParser parser = parseEd(parse);
    final PrologTerm term = parser.next();
    assertFalse(parser.hasNext());
    assertEquals(TermType.ATOM, term.getTermType());
    assertEquals(1, term.getLine());
    assertEquals(1, term.getPos());
    assertTrue(term instanceof PrologInteger);
    assertEquals(expected, ((PrologInteger) term).getNumber().longValue());
  }

  @Test
  public void testParseZeroSingleQuoteChar() {
    assertPrologInteger('a', "0'a.");
    assertPrologInteger(' ', "0'\\s.");
    assertPrologInteger('\u1234', "0'\\u1234.");
    assertPrologInteger('\n', "0'\\n.");
    assertPrologInteger('\"', "0'\".");
    assertPrologInteger('\'', "0'\'.");
    assertPrologInteger('`', "0'`.");
    checkWrongClauseReadingWithPPE("0'ab.", 4);
  }

  @Test
  public void testVariableMustBeNotEqualAtClauseBounds() {
    PrologStruct structure = (PrologStruct) parseEd("test(A,B,C,A,B,C,A,B,C,A,B,C,_,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,_,_).").next();

    final Set<PrologVariable> varSet = new HashSet<>();
    for (int li = 0; li < structure.getArity(); li++) {
      final PrologVariable currentVar = (PrologVariable) structure.getElementAt(li);
      assertFalse(varSet.contains(currentVar));
      varSet.add(currentVar);
    }

    assertEquals(structure.getArity(), varSet.size());
  }

  @Test
  public void testVariablesAtClauseBounds() {
    PrologStruct structure = (PrologStruct) parseEd("test(A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C).").next();

    final PrologVariable varA = (PrologVariable) structure.getElementAt(0);
    final PrologVariable varB = (PrologVariable) structure.getElementAt(1);
    final PrologVariable varC = (PrologVariable) structure.getElementAt(2);

    assertNotSame(varA, varB);
    assertNotSame(varA, varC);
    assertNotSame(varB, varC);

    assertEquals("A", varA.getTermText());
    assertEquals("B", varB.getTermText());
    assertEquals("C", varC.getTermText());

    for (int li = 3; li < structure.getArity(); ) {
      assertNotSame(varA, structure.getElementAt(li++));
      assertNotSame(varB, structure.getElementAt(li++));
      assertNotSame(varC, structure.getElementAt(li++));
    }
  }

  @Test
  public void testEndOfStream() {
    final PrologParser parser = parseEd("hello.world.");
    PrologTerm term = parser.next();

    assertEquals(ATOM, term.getTermType());
    assertEquals("hello", term.getTermText());

    term = parser.next();
    assertEquals(ATOM, term.getTermType());
    assertEquals("world", term.getTermText());

    assertThrows(NoSuchElementException.class, () -> parser.next());
  }

  private void checkWrongClauseReadingWithPPE(final String readClause, final int stringPosition) {
    assertEquals(stringPosition, assertThrows(PrologParserException.class, () -> parseEd(readClause).next()).getPos());
  }

  @Test
  public void testErrorListDefinitions() {
    checkWrongClauseReadingWithPPE(" [,].", 3);
    checkWrongClauseReadingWithPPE(" [|].", 2);
    checkWrongClauseReadingWithPPE(" [345|].", 7);
    checkWrongClauseReadingWithPPE(" [345|323|X].", 11);
    checkWrongClauseReadingWithPPE(" [345|323|].", 10);
    checkWrongClauseReadingWithPPE(" [|345].", 2);
    checkWrongClauseReadingWithPPE(" [1,2,3.", 8);
  }

  @Test
  public void testQuoting_Wrong() {
    checkWrongClauseReadingWithPPE("\"abc'.", 1);
    checkWrongClauseReadingWithPPE("'abc\".", 1);
    checkWrongClauseReadingWithPPE("`abc\".", 1);
    checkWrongClauseReadingWithPPE("`abc'.", 1);
  }

  @Test
  public void testQuoting() {
    checkParseAtomQuoting("test.", "test", NO_QUOTED);
    checkParseAtomQuoting("\'t\"es`t\'.", "'t\"es`t'", SINGLE_QUOTED);
    checkParseAtomQuoting("`t'\\`e\'s\"t`.", "`t'\\`e's\"t`", BACK_QUOTED);
    checkParseAtomQuoting("\"t`e\'s\\\"\".", "\"t`e's\\\"\"", DOUBLE_QUOTED);
  }

  private void checkParseAtomWithoutPPE(final String atomToBeChecked, final String expectedAtomText) {
    final PrologTerm atom = parseEd(atomToBeChecked + '.').next();
    assertEquals(ATOM, atom.getTermType());
    assertEquals(PrologAtom.class, atom.getClass());
    assertEquals(expectedAtomText, atom.getTermText());
  }

  private void checkParseAtomQuoting(final String atomToBeChecked, final String expectedAtomText, final PrologTerm.QuotingType expectedType) {
    final PrologTerm atom = parseEd(atomToBeChecked + '.').next();
    assertEquals(ATOM, atom.getTermType());
    assertEquals(expectedType, atom.getQuotingType());
    assertEquals(PrologAtom.class, atom.getClass());
    assertEquals(expectedAtomText, atom.toString());
  }

  @Test
  public void testParseAtom() {
    checkParseAtomWithoutPPE("a", "a");
    checkParseAtomWithoutPPE("test012", "test012");
    checkParseAtomWithoutPPE("x______y", "x______y");
    checkParseAtomWithoutPPE("alpha_beta_procedure", "alpha_beta_procedure");
    // test of non-latin chars, "hello" in russian
    checkParseAtomWithoutPPE("привет", "привет");
    checkParseAtomWithoutPPE("miss_Jones", "miss_Jones");
    checkParseAtomWithoutPPE("\'Jones\'", "Jones");
    checkParseAtomWithoutPPE("\'\'", "");
    checkParseAtomWithoutPPE("x_", "x_");
  }

  private void checkIntegerWithoutPPE(final String atomToBeChecked, final long expectedNumber) {
    PrologTerm atom = parseEd(atomToBeChecked + '.').next();
    assertEquals(ATOM, atom.getTermType(), "Type: " + atom.getTermType());
    assertEquals(PrologInteger.class, atom.getClass(), "Class: " + atom.getClass());
    assertEquals(expectedNumber, ((PrologInteger) atom).getNumber().longValue(), "Number: " + ((PrologInteger) atom).getNumber().longValue());
    assertEquals(Long.toString(expectedNumber), atom.getTermText(), "Text: " + atom.getTermText());
  }

  @Test
  public void testParseInteger() {
    assertThrows(PrologParserException.class, () -> parseEd("1_000_/*more*/000").next());
    checkIntegerWithoutPPE("-17", -17);
    checkIntegerWithoutPPE("0", 0);
    checkIntegerWithoutPPE("1", 1);
    checkIntegerWithoutPPE("1313", 1313);
    checkIntegerWithoutPPE("-0", 0);
    checkIntegerWithoutPPE("-97", -97);
    checkIntegerWithoutPPE("-97", -97);
    checkIntegerWithoutPPE(Long.toString(Long.MAX_VALUE), Long.MAX_VALUE);

    checkIntegerWithoutPPE(Long.toString(Long.MIN_VALUE), Long.MIN_VALUE);

    final PrologTerm val = parseEd("'298723987'.").next();
    assertEquals(ATOM, val.getTermType());
    assertEquals(PrologAtom.class, val.getClass());
    assertEquals("298723987", val.getTermText());
  }

  private void checkFloatWithoutPPE(final String atomToBeChecked, final double expectedNumber) {
    final PrologTerm atom = parseEd(atomToBeChecked + '.').next();
    assertEquals(ATOM, atom.getTermType());
    assertEquals(PrologFloat.class, atom.getClass());
    assertEquals(expectedNumber, ((PrologFloat) atom).getNumber().doubleValue(), Double.MIN_NORMAL, String.format("%e <> %e", expectedNumber, ((PrologFloat) atom).getNumber().doubleValue()));
    assertEquals(BigDecimal.valueOf(expectedNumber).toEngineeringString(), atom.getTermText());
  }

  @Test
  public void testParseFloat() {
    checkFloatWithoutPPE(new BigDecimal(Math.PI, PrologFloat.MATH_CONTEXT).toEngineeringString(), Math.PI);
    checkFloatWithoutPPE("-2.67e+021", -2.67e+021);
    checkFloatWithoutPPE("-0.0035", -0.0035d);
    checkFloatWithoutPPE("100.2", 100.2d);
    checkFloatWithoutPPE("2000.0", 2.0e+3d);

    final PrologTerm val = parseEd("298723987493287423423.00002342342300043324234324E+75.").next();
    assertEquals(ATOM, val.getTermType());
    assertEquals(PrologFloat.class, val.getClass());

    final PrologTerm valAtom = parseEd("2.0E.").next();
    assertEquals(ATOM, valAtom.getTermType());
    assertEquals(PrologAtom.class, valAtom.getClass());
  }

  @Test
  public void testParseVariable() {
    PrologTerm var = parseEd("X.").next();
    assertEquals(TermType.VAR, var.getTermType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("X", var.getTermText());

    var = parseEd("Result.").next();
    assertEquals(TermType.VAR, var.getTermType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("Result", var.getTermText());

    var = parseEd("Object2.").next();
    assertEquals(TermType.VAR, var.getTermType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("Object2", var.getTermText());

    var = parseEd("Participant_list.").next();
    assertEquals(TermType.VAR, var.getTermType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("Participant_list", var.getTermText());

    var = parseEd("ShoppingList.").next();
    assertEquals(TermType.VAR, var.getTermType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("ShoppingList", var.getTermText());

    var = parseEd("_x23.").next();
    assertEquals(TermType.VAR, var.getTermType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("_x23", var.getTermText());

    var = parseEd("_23.").next();
    assertEquals(TermType.VAR, var.getTermType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("_23", var.getTermText());
  }

  @Test
  public void testParseStructure() {
    final ParserContext mockContext = mock(ParserContext.class);
    when(mockContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    PrologParser parser = parseEd("date(Day,may,2001).", mockContext);
    final PrologTerm term = parser.next();

    assertEquals(TermType.STRUCT, term.getTermType());

    PrologStruct struct = (PrologStruct) term;
    assertEquals(ATOM, struct.getFunctor().getTermType());
    assertEquals("date", struct.getFunctor().getTermText());
    assertEquals(3, struct.getArity());
    assertEquals(TermType.VAR, struct.getElementAt(0).getTermType());
    assertEquals("Day", struct.getElementAt(0).getTermText());
    assertEquals(ATOM, struct.getElementAt(1).getTermType());
    assertEquals("may", struct.getElementAt(1).getTermText());
    assertEquals(ATOM, struct.getElementAt(2).getTermType());
    assertEquals(2001L,
        ((PrologInteger) struct.getElementAt(2)).getNumber().longValue());
  }

  @Test
  public void testParseList() {
    PrologList list = (PrologList) parseEd("[].").next();
    assertEquals(TermType.LIST, list.getTermType());
    assertEquals(PrologList.class, list.getClass());
    assertTrue((list).isEmpty());

    list = (PrologList) parseEd("[1,2,3,4,5].").next();
    assertFalse(list.isEmpty());
    assertEquals("[1, 2, 3, 4, 5]", list.toString());

    list = (PrologList) parseEd("[Hello|World].").next();
    assertFalse(list.isEmpty());
    assertEquals("[Hello|World]", list.toString());
    assertEquals(TermType.VAR, list.getHead().getTermType());
    assertEquals("Hello", list.getHead().getTermText());
    assertEquals(TermType.VAR, list.getTail().getTermType());
    assertEquals("World", list.getTail().getTermText());

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
  public void testParseOperator() {
    final ParserContext mockContext = mock(ParserContext.class);
    when(mockContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    PrologParser parser = parseEd("hello:-world.", mockContext);

    PrologStruct struct = (PrologStruct) parser.next();

    assertEquals(PrologStruct.class, struct.getClass());
    assertEquals(TermType.OPERATOR, struct.getFunctor().getTermType());
    assertEquals(":-", struct.getFunctor().getTermText());
    assertEquals(2, struct.getArity());
    assertEquals(OpAssoc.XFX,
        ((Op) struct.getFunctor()).getOpAssoc());
    assertEquals("hello", struct.getElementAt(0).getTermText());
    assertEquals("world", struct.getElementAt(1).getTermText());

    clearInvocations(mockContext);
    parser = parseEd(":-test.", mockContext);
    struct = (PrologStruct) parser.next();

    assertEquals(PrologStruct.class, struct.getClass());
    assertEquals(TermType.OPERATOR, struct.getFunctor().getTermType());
    assertEquals(":-", struct.getFunctor().getTermText());
    assertEquals(1, struct.getArity());
    assertEquals(OpAssoc.FX,
        ((Op) struct.getFunctor()).getOpAssoc());
    assertEquals("test", struct.getElementAt(0).getTermText());

    clearInvocations(mockContext);
    parser = parseEd("X is X+1.", mockContext);
    struct = (PrologStruct) parser.next();

    assertEquals(PrologStruct.class, struct.getClass());
    assertEquals(TermType.OPERATOR, struct.getFunctor().getTermType());
    assertEquals("is", struct.getFunctor().getTermText());
    assertEquals(2, struct.getArity());
    assertEquals(OpAssoc.XFX,
        ((Op) struct.getFunctor()).getOpAssoc());
    assertEquals("X", struct.getElementAt(0).getTermText());
    assertEquals(TermType.STRUCT, struct.getElementAt(1).getTermType());
    assertEquals("+", ((PrologStruct) struct.getElementAt(1)).getFunctor().getTermText());
    assertEquals(OpAssoc.YFX,
        ((Op) ((PrologStruct) struct.getElementAt(1)).getFunctor()).getOpAssoc());
    assertEquals(2, ((PrologStruct) struct.getElementAt(1)).getArity());
    assertEquals("X", ((PrologStruct) struct.getElementAt(1)).getElementAt(0).getTermText());
    assertEquals("1", ((PrologStruct) struct.getElementAt(1)).getElementAt(1).getTermText());

    assertEquals("+ - + a", parseEd("+-+a.").next().toString());
    assertEquals("+ - + 1", parseEd("+-+1.").next().toString());
    assertEquals("+ -1", parseEd("+-1.").next().toString());
    assertEquals("X is 2 + 3 * 5", parseEd("X is 2+3*5.").next().toString());
  }

  @Test
  public void testComments() {
    final PrologParser parser = parseEd("  %   zero line %%%% misc%%%% \n%first line\n%second line\n\nhello:-world.%test\n:-test.");
    PrologTerm term = parser.next();
    PrologTerm term2 = parser.next();

    assertEquals("hello :- world", term.toString());
    assertEquals(":- test", term2.toString());

    assertFalse(parser.hasNext());
  }


  @Test
  public void testSimilarOperatorInterpretation() {
    final Map<String, OpContainer> operators = new HashMap<>();
    final Op firstOperator = Op.make(100, OpAssoc.FY, "++++++");
    final Op secondOperator = Op.make(100, OpAssoc.FY, "+++");
    final Op thirdOperator = Op.make(100, OpAssoc.FY, "++");

    operators.put(firstOperator.getTermText(), make(firstOperator));
    operators.put(secondOperator.getTermText(), make(secondOperator));
    operators.put(thirdOperator.getTermText(), make(thirdOperator));

    final StubContext contextStub = new StubContext(operators);
    final PrologTerm term = parseEd("+++++++++++ 3.", contextStub).next();

    assertSame(firstOperator, ((PrologStruct) term).getFunctor());
    assertSame(secondOperator, ((PrologStruct) (((PrologStruct) term).getElementAt(0))).getFunctor());
    assertSame(thirdOperator, ((PrologStruct) ((PrologStruct) (((PrologStruct) term).getElementAt(0))).getElementAt(0)).getFunctor());

  }

  @Test
  public void testOperatorHierarchy() {
    final Map<String, OpContainer> operators = new HashMap<>();

    final StubContext contextStub = new StubContext(operators);
    final PrologParser parser = parseEd(":-op(800,xfx,'<===>').:-op(700,xfy,'v').:-op(600,xfy,'&').:-op(500,fy,'~').~(A&B)<===> ~A v ~B.", contextStub);

    PrologTerm term = null;

    while (parser.hasNext()) {
      term = parser.next();

      if (term.getTermType() == TermType.STRUCT) {
        final PrologStruct structure = (PrologStruct) term;
        if (structure.getArity() == 1 && structure.getFunctor().getTermText().equals(":-")) {
          final PrologStruct operatorstructure = (PrologStruct) structure.getElementAt(0);

          if (operatorstructure.getArity() == 3 && operatorstructure.getFunctor().getTermText().equals("op")) {
            final Op newoperator = Op.make(
                ((PrologInteger) operatorstructure.getElementAt(0)).getNumber().intValue(),
                OpAssoc.findForName(operatorstructure.getElementAt(1).getTermText()).get(),
                operatorstructure.getElementAt(2).getTermText());

            OpContainer container = operators.get(newoperator.getTermText());

            if (container == null) {
              container = make(newoperator);
              operators.put(newoperator.getTermText(), container);
            } else {
              container.add(newoperator);
            }

          } else {
            fail("Unsupported structure detected");
          }
        }
      }
    }

    assertEquals(TermType.STRUCT, term.getTermType());
    assertEquals("<===>", term.getTermText());
    assertEquals(2, ((PrologStruct) term).getArity());
    assertEquals(800, term.getPrecedence());

    final PrologStruct leftBranch = (PrologStruct) ((PrologStruct) term).getElementAt(0);
    final PrologStruct rightBranch = (PrologStruct) ((PrologStruct) term).getElementAt(1);

    assertEquals(500, leftBranch.getPrecedence());
    assertEquals("~", leftBranch.getTermText());
    assertEquals(1, leftBranch.getArity());

    assertEquals(700, rightBranch.getPrecedence());
    assertEquals("v", rightBranch.getTermText());
    assertEquals(2, rightBranch.getArity());

    final PrologStruct leftBranch2 = (PrologStruct) leftBranch.getElementAt(0);
    final PrologStruct rightBranchL = (PrologStruct) rightBranch.getElementAt(0);
    final PrologStruct rightBranchR = (PrologStruct) rightBranch.getElementAt(1);

    assertEquals(600, leftBranch2.getPrecedence());
    assertEquals("&", leftBranch2.getTermText());
    assertEquals(2, leftBranch2.getArity());
    assertEquals("A", leftBranch2.getElementAt(0).getTermText());
    assertEquals("B", leftBranch2.getElementAt(1).getTermText());

    assertEquals(500, rightBranchL.getPrecedence());
    assertEquals("~", rightBranchL.getTermText());
    assertEquals(1, rightBranchL.getArity());
    assertEquals("A", rightBranchL.getElementAt(0).getTermText());

    assertEquals(500, rightBranchR.getPrecedence());
    assertEquals("~", rightBranchR.getTermText());
    assertEquals(1, rightBranchR.getArity());
    assertEquals("B", rightBranchR.getElementAt(0).getTermText());

    assertEquals("~ (A & B) <===> ~ A v ~ B", term.toString());
  }

  private void assertReadTerms(final int expected, final String resource, final Op... ops) {
    final ParserContext defaultContext = of(ParserContext.FLAG_BLOCK_COMMENTS, ops);
    try (Reader reader = new InputStreamReader(getClass().getResourceAsStream(resource), StandardCharsets.UTF_8)) {
      final PrologParser parser = parseEd(reader, defaultContext);
      assertEquals(expected, parser.stream().count());
    } catch (IOException ex) {
      ex.printStackTrace();
      fail("IOException");
    }
  }

  @Test
  public void testParseSourceFiles() {
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
    assertReadTerms(75, "sictus.pl", Op.make(900, OpAssoc.XFX, "=>"), Op.make(800, XFY, "&"), Op.make(300, OpAssoc.XFX, ":"));
  }

  @Test
  public void testOperatorLooksAsAtomAndPartOfAtom() {
    assertEquals("module", parseEd("module.").next().getTermText());
  }

  @Test
  public void testStreamPositionForTerms() {
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

    final PrologStruct mainterm = (PrologStruct) parseEd("  %\ncube (X,\'hello\',1000) :- \n Y is X * X * X.").next();

    // ':-'
    assertEquals(23, mainterm.getPos());
    assertEquals(2, mainterm.getLine());

    final PrologStruct leftPart = (PrologStruct) mainterm.getElementAt(0);
    // 'cube(X,Y)'
    assertEquals(1, leftPart.getPos());
    assertEquals(2, leftPart.getLine());
    // 'X'
    assertEquals(7, leftPart.getElementAt(0).getPos());
    assertEquals(2, leftPart.getElementAt(0).getLine());
    // 'hello'
    assertEquals(9, leftPart.getElementAt(1).getPos());
    assertEquals(2, leftPart.getElementAt(1).getLine());
    // 1000
    assertEquals(17, leftPart.getElementAt(2).getPos());
    assertEquals(2, leftPart.getElementAt(2).getLine());

    final PrologStruct rightPart = (PrologStruct) mainterm.getElementAt(1);
    // 'is'
    assertEquals(4, rightPart.getPos());
    assertEquals(3, rightPart.getLine());
    // 'Y'
    assertEquals(2, rightPart.getElementAt(0).getPos());
    assertEquals(3, rightPart.getElementAt(0).getLine());

    final PrologStruct structure = (PrologStruct) parseEd("test(\n1,  \n2, 4*2,  \n3,4,\n5,6*7,8).").next();
    // 4*2
    assertEquals(5, structure.getElementAt(2).getPos());
    assertEquals(3, structure.getElementAt(2).getLine());
    // 6*7
    assertEquals(4, structure.getElementAt(6).getPos());
    assertEquals(5, structure.getElementAt(6).getLine());
  }

  private void assertZFZOperatorStruct(final String operator, final PrologTerm term) {
    assertEquals(TermType.STRUCT, term.getTermType());
    final PrologStruct struct = (PrologStruct) term;
    assertEquals(2, struct.getArity());
    assertEquals(TermType.OPERATOR, struct.getFunctor().getTermType());
    assertEquals(operator, struct.getFunctor().getTermText());
  }

  @Test
  public void testPairOperatorsWithoutWhitespaces() {
    final PrologParser parser = parseEd("a=..b.a..b.", of(FLAG_NONE, Op.SWI_CPL));
    assertZFZOperatorStruct("=..", parser.next());
    assertZFZOperatorStruct("..", parser.next());
    assertFalse(parser.hasNext());
  }

  @Test
  public void testSingleOperatorAsAtom() {
    final PrologStruct structure = (PrologStruct) parseEd("not/stream.").next();
    assertZFZOperatorStruct("/", structure);
    assertEquals(ATOM, structure.getElementAt(0).getTermType());
    assertEquals("not", structure.getElementAt(0).getTermText());
    assertEquals(ATOM, structure.getElementAt(1).getTermType());
    assertEquals("stream", structure.getElementAt(1).getTermText());
  }

  @Test
  public void testRecognizingUserOperatorsWhichSimilarMetaOperators() {
    final Map<String, OpContainer> operators = new HashMap<>();
    operators.put("(((", make(Op.make(1, OpAssoc.FX, "(((")));
    operators.put("...", make(Op.make(1200, OpAssoc.XF, "...")));
    final StubContext stubContext = new StubContext(operators);

    final PrologStruct structure = (PrologStruct) parseEd("(((hello....", stubContext).next();
    assertEquals("...", structure.getFunctor().getTermText());
    assertEquals("(((", ((PrologStruct) structure.getElementAt(0)).getFunctor().getTermText());
    assertEquals("hello", ((PrologStruct) structure.getElementAt(0)).getElementAt(0).getTermText());
  }

  @Test
  public void testOperatorNameAsAtomicFunctor() {
    final PrologStruct structure = (PrologStruct) parseEd("'mod'(_,_,_,_).").next();
    assertEquals("mod", structure.getFunctor().getTermText());
    assertNotSame(structure.getFunctor().getTermType(), TermType.OPERATOR);
    assertEquals(4, structure.getArity());
  }

  @Test
  public void testOperatorNameAsFunctor_EmptyBrackets() {
    final PrologParserException ex = assertThrows(PrologParserException.class, () -> parseEd("+().").next());
    assertEquals(2, ex.getPos());
    assertEquals(1, ex.getLine());
  }

  @Test
  public void testAtomAsFunctor_EmptyBrackets() {
    final PrologParserException ex = assertThrows(PrologParserException.class, () -> parseEd("'hello'().").next());
    assertEquals(8, ex.getPos());
    assertEquals(1, ex.getLine());
  }

  @Test
  public void testAtomInSequenceWithNameSimilarOperator() {
    final PrologStruct structure = (PrologStruct) parseEd("functor(1,2,3,'mod',5,6).").next();
    assertEquals(6, structure.getArity());
    assertEquals("1", structure.getElementAt(0).getTermText());
    assertEquals("2", structure.getElementAt(1).getTermText());
    assertEquals("3", structure.getElementAt(2).getTermText());
    assertEquals("mod", structure.getElementAt(3).getTermText());
    assertEquals("5", structure.getElementAt(4).getTermText());
    assertEquals("6", structure.getElementAt(5).getTermText());
  }

  @Test
  public void testVeryLongList() {
    final int ELEMENTS = 100000;

    final StringBuilder buffer = new StringBuilder(ELEMENTS);

    buffer.append('[');
    boolean nonFirst = false;

    for (int i = 0; i < ELEMENTS; i++) {
      if (nonFirst) {
        buffer.append(',');
      } else {
        nonFirst = true;
      }
      buffer.append(i);
    }
    buffer.append("].");

    PrologList list = (PrologList) parseEd(buffer.toString()).next();

    for (int i = 0; i < ELEMENTS; i++) {
      final PrologInteger head = (PrologInteger) list.getHead();
      assertEquals(i, head.getNumber().intValue());
      list = (PrologList) list.getTail();
    }

    assertTrue(list.isEmpty());
  }

  @Test
  public void testHelloWorld() {
    final PrologStruct rule = (PrologStruct) parseEd("hello :- world,!.").next();
    final PrologStruct and = (PrologStruct) rule.getElementAt(1);
    assertEquals("hello world!", rule.getElementAt(0).getTermText() + ' ' + and.getElementAt(0).getTermText() + and.getElementAt(1).getTermText());
  }

  @Test
  public void testVeryLongStructure() {
    final int ELEMENTS = 100000;

    final StringBuilder buffer = new StringBuilder(ELEMENTS);
    buffer.append("test(");
    boolean nonfirst = false;
    for (int i = 0; i < ELEMENTS; i++) {
      if (nonfirst) {
        buffer.append(',');
      } else {
        nonfirst = true;
      }
      buffer.append(i - 100);
    }
    buffer.append(").");

    PrologStruct struct = (PrologStruct) parseEd(buffer.toString()).next();

    assertEquals(ELEMENTS, struct.getArity());
    assertEquals("test", struct.getFunctor().getTermText());
    for (int i = 0; i < ELEMENTS; i++) {
      assertEquals(i - 100, ((PrologInteger) struct.getElementAt(i)).getNumber().intValue());
    }
  }

  @Test
  public void testParserStream() {
    PrologParser parser = parseEd("z(some).a(X):-[X].b('\\'hello world\\'').list([_|Tail]) :- list(Tail).");
    final String joined = parser.stream().map(x -> x.toString()).collect(joining(". ", "", "."));
    parser = parseEd(joined);
    assertEquals(joined, parser.stream().map(x -> x.toString()).collect(joining(". ", "", ".")));
    assertEquals("z(some). a(X) :- [X]. b('\\'hello world\\''). list([_|Tail]) :- list(Tail).", joined);
  }

  @Test
  public void testStructStream() {
    PrologParser parser = parseEd("s(1,2,3,4,5,6,7,8).");
    assertEquals("1\n" +
        "2\n" +
        "3\n" +
        "4\n" +
        "5\n" +
        "6\n" +
        "7\n" +
        "8", parser.next().stream().map(x -> x.toString()).collect(joining("\n")));
  }

  @Test
  public void testListStream() {
    PrologParser parser = parseEd("[1,2,3,4,5,6,7,8|_].");
    assertEquals("1\n" +
        "2\n" +
        "3\n" +
        "4\n" +
        "5\n" +
        "6\n" +
        "7\n" +
        "8\n" +
        "_", parser.next().stream().map(x -> x.toString()).collect(joining("\n")));
  }


  @Test
  public void testTermStreamFlat() {
    PrologParser parser = parseEd("some(hello,world,[1,2,3|X],end).");
    final String joined = parser.stream()
        .flatMap(PrologTerm::stream)
        .flatMap(PrologTerm::stream)
        .map(PrologTerm::toString)
        .collect(joining("\n"));

    assertEquals("hello\n" +
        "world\n" +
        "1\n" +
        "2\n" +
        "3\n" +
        "X\n" +
        "end", joined);
  }

  private String parseSortAndJoin(final String text) {
    return parseEd(text).stream().sorted().map(PrologTerm::toString).collect(joining("\n"));
  }

  @Test
  public void testStringWithIsoControl() {
    assertEquals("'hello→world'", parseSortAndJoin("'hello\u0000world'."));
    assertEquals("'hello→world'", parseSortAndJoin("'hello\u0001world'."));
    assertEquals("'hello\\nworld'", parseSortAndJoin("'hello\\\nworld'."));
    assertEquals("'hello\\nworld'", parseSortAndJoin("'hello\\\r\nworld'."));
  }

  @Test
  public void testSignedNumerics_EdingburghParser() {
    assertEquals(-1, ((PrologNumeric) parseEd("-1.").next()).getNumber().intValue());
    assertEquals(-1.1f, ((PrologNumeric) parseEd("-1.1.").next()).getNumber().floatValue(), Float.MIN_NORMAL);
    assertEquals(1, ((PrologNumeric) parseEd("1.").next()).getNumber().intValue());
    assertEquals(1.1f, ((PrologNumeric) parseEd("1.1.").next()).getNumber().floatValue(), Float.MIN_NORMAL);
  }

  @Test
  public void testSignedNumerics_GenericParser() {
    assertThrows(PrologParserException.class, () -> parseGen("-1.").next());
    assertThrows(PrologParserException.class, () -> parseGen("+1.").next());
    assertThrows(PrologParserException.class, () -> parseGen("-1.1.").next());
    assertThrows(PrologParserException.class, () -> parseGen("+1.1.").next());

    assertEquals(1, ((PrologNumeric) parseGen("1.").next()).getNumber().intValue());
    assertEquals(1.1f, ((PrologNumeric) parseGen("1.1.").next()).getNumber().floatValue(), Float.MIN_NORMAL);

    assertEquals(-1, ((PrologNumeric) parseGen("-1.", of(FLAG_NONE, Op.ISO_UNARY_MINUS)).next()).getNumber().intValue());
    assertEquals("+ 1", parseGen("+1.", of(FLAG_NONE, Op.SWI_UNARY_PLUS)).next().toString());
    assertEquals("+ 1.1", parseGen("+1.1.", of(FLAG_NONE, Op.SWI_UNARY_PLUS)).next().toString());
    assertEquals(-1.1f, ((PrologNumeric) parseGen("-1.1.", of(FLAG_NONE, Op.ISO_UNARY_MINUS)).next()).getNumber().floatValue(), Float.MIN_NORMAL);
  }

  @Test
  public void testMisc() throws Exception {
    assertNotNull(parseEd("''(123_345)**[].").next());
    assertEquals("'+'", parseEd("+.").next().toString());
    assertEquals("some(a, '+', b)", parseEd("some(a,+,b).").next().toString());
    assertEquals("some('+', '+', '+')", parseEd("some(+,+,+).").next().toString());
  }

  @Test
  public void testParseBigGeneratedPrologSource() throws Exception {
    final int CLAUSES = 1000;
    assertEquals(CLAUSES, new GenericPrologParser(new InputStreamReader(new PrologSourceKoi7Generator(CLAUSES, true)), DefaultParserContext.of(FLAG_NONE, Op.SWI)).stream().count());
    assertEquals(CLAUSES, new GenericPrologParser(new InputStreamReader(new PrologSourceKoi7Generator(CLAUSES, false)), DefaultParserContext.of(FLAG_NONE, Op.SWI)).stream().count());
  }

  @Test
  public void testNonCompletedClause() throws Exception {
    assertThrows(PrologParserException.class, () -> parseEd("a").next());
    assertThrows(PrologParserException.class, () -> parseEd("123").next());
    assertThrows(PrologParserException.class, () -> parseEd("\"dsdsd\"").next());
    assertThrows(PrologParserException.class, () -> parseEd("some(1,2)").next());
    assertThrows(PrologParserException.class, () -> parseEd("[1,2,3,4]").next());
    assertThrows(PrologParserException.class, () -> parseEd("[1,2,3").next());
    assertThrows(PrologParserException.class, () -> parseEd("a(").next());
    assertThrows(PrologParserException.class, () -> parseEd("1.22").next());
  }

  private void assertOperatorAsFunctor(final String expected, final PrologTerm term) {
    assertTrue(term instanceof PrologStruct);
    final PrologStruct struct = (PrologStruct) term;
    assertEquals(TermType.OPERATOR, struct.getFunctor().getTermType());
    assertEquals(((Op) struct.getFunctor()).getArity(), struct.getArity());
    assertEquals(expected, term.toString());
  }

  @Test
  public void testOperatorAsFunctor() throws Exception {
    assertOperatorAsFunctor("1 + 2", parseEd("+(1,2).").next());
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
  public void testPairOfOperatorsWithIncompatiblePriority() throws Exception {
    assertEquals("-(discontiguous)", parseEd("-discontiguous.").next().toString());
    assertEquals("a : b :> c :> d", parseEd("a:b:>c:>d.", DefaultParserContext.of(ParserContext.FLAG_NONE, Op.make(500, XFY, ":>"))).next().toString());
    assertThrows(PrologParserException.class, () -> parseEd("a :- b :- c.").next());
    assertThrows(PrologParserException.class, () -> parseEd("?-mother(pam,bob);").next());
  }

  @Test
  public void testOperatorAsFunctorWithUnsupportedArity() throws Exception {
    assertEquals("'+'(1, 2, 3)", parseEd("+(1,2,3).").next().toString());
    assertEquals("':'(1, 2, 3)", parseEd(":(1,2,3).").next().toString());
    assertEquals("':'(1, (2 , 3), 4)", parseEd(":(1,(2,3),4).").next().toString());
    assertEquals("':'(1)", parseEd(":(1).").next().toString());
  }

  @Test
  public void testBlock() {
    assertEquals("(1)", parseEd("(1).").next().toString());
    assertEquals("(1 , 2)", parseEd("(1,2).").next().toString());
    assertEquals("(1 - 2)", parseEd("(1-2).").next().toString());
    assertEquals("(1 - 2 * (34 / 33))", parseEd("(1-2*(34/33)).").next().toString());
    assertEquals("(1 , 2 , 3)", parseEd("(1,2,3).").next().toString());
    assertEquals("((1 , 2) , 3)", parseEd("((1,2),3).").next().toString());
    assertEquals("((1 , 2) , 3)", parseEd("(((1,2),3)).").next().toString());
    assertEquals("[A|(B | C)]", parseEd("[A|((((B|C))))].").next().toString());
    assertEquals("vertical(line(point(X, Y), point(X, Z)))", parseEd("vertical(line(point(X,Y), point(X,Z))).").next().toString());
    assertEquals("move(state(middle, onbox, middle, hasnot), grasp, state(middle, onbox, middle, has))", parseEd("move(state(middle, onbox, middle, hasnot),grasp,state(middle, onbox, middle, has)).").next().toString());
    assertEquals("X1 =< X2 , X2 =< (X1 + LENGTH) , (Y1 - LENGTH) =< Y2 , Y2 =< Y1", parseEd("X1 =< X2, X2 =< (X1 + LENGTH), (Y1-LENGTH) =< Y2, Y2 =< Y1.").next().toString());
  }

  @Test
  public void testAloneOperatorAsAtom() throws Exception {
    assertEquals(TermType.ATOM, parseEd("/.").next().getTermType());
    assertEquals(TermType.ATOM, parseEd("-.").next().getTermType());
    assertEquals(TermType.ATOM, parseEd("+.").next().getTermType());
  }

  @Test
  public void testUnexpectedlyEndedReadStream() throws Exception {
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
                    false)), new DefaultParserContext(FLAG_BLOCK_COMMENTS));

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
  public void testStandardTermOrder() {
    assertEquals("1\n2\n3\n4\n5", parseSortAndJoin("5. 3. 1. 2. 4."));
    assertEquals("1.3\n2\n3.6\n4\n5.23", parseSortAndJoin("5.23. 3.6. 1.3. 2. 4."));
    assertEquals("1.3\n2\n3.6\n4\n5.23", parseSortAndJoin("5.23.3.6.1.3.2. 4."));
    assertEquals("a\nb\nc\nd\ne", parseSortAndJoin("c.e.b.a.d."));
    assertEquals("[]\n[1, 2]\n[1, 2, 3]\n[5, 6, 7]\n[8]", parseSortAndJoin("[1,2,3].[1,2].[].[5,6,7].[8]."));
    assertEquals("u(8)\nl(1, 2)\ng(5, 6, 7)\nh(1, 2, 3)", parseSortAndJoin("h(1,2,3).l(1,2).g(5,6,7).u(8)."));
    assertEquals("X\nY\n_\n34.112\n112\natm\n[]\nm(34)\n[1, 2, 3]\na(1, 2)", parseSortAndJoin("a(1,2).[1,2,3].X.112.Y.34.112. atm. [].m(34)._."));
  }

  private static class StubContext implements ParserContext {

    private final Map<String, OpContainer> operators;

    public StubContext(final Map<String, OpContainer> operators) {
      this.operators = operators;
    }

    @Override
    public Map<String, OpContainer> findAllOperators() {
      return Collections.emptyMap();
    }

    @Override
    public int getMaxTokenizerBufferLength() {
      return 1000;
    }

    @Override
    public int getFlags() {
      return FLAG_NONE;
    }

    @Override
    public boolean hasOpStartsWith(final PrologParser source,
                                   final String operatorNameStartSubstring) {
      for (final String string : operators.keySet()) {
        if (string.startsWith(operatorNameStartSubstring)) {
          return true;
        }
      }

      return false;
    }

    @Override
    public OpContainer findOpForName(final PrologParser source,
                                     final String operatorName) {
      return operators.get(operatorName);
    }

  }

}
