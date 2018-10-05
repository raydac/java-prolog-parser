package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloatNumber;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.terms.PrologVariable;
import com.igormaznitsa.prologparser.tokenizer.PrologParser;
import com.igormaznitsa.prologparser.utils.StringUtils;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import static com.igormaznitsa.prologparser.operators.OperatorContainer.newOpCont;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class IntegrationTest {

  private EdinburghPrologParser parserFor(final String str) {
    final ParserContext mock = mock(ParserContext.class);
    return new EdinburghPrologParser(new StringReader(str), mock);
  }

  @Test
  public void testParseStringWithSpecialChars() {
    final PrologParser parser = parserFor("'\u0008Hello\\\nWorld\u0021\\r'.'\\xFF\\Another String\u0007'.");
    AbstractPrologTerm term = parser.next();

    assertEquals(PrologTermType.ATOM, term.getType());
    assertEquals("\\bHello\\nWorld!\\r",
        StringUtils.escapeString(term.getText()));

    term = parser.next();
    assertNotNull(term);
    assertEquals("\u00FFAnother String\u0007", term.getText());

  }

  @Test
  public void testVariableMustBeNotEqualAtSentenceBounds() {
    PrologStructure structure = (PrologStructure) parserFor("test(A,B,C,A,B,C,A,B,C,A,B,C,_,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,_,_).").next();

    final Set<PrologVariable> varSet = new HashSet<>();
    for (int li = 0; li < structure.getArity(); li++) {
      final PrologVariable currentVar = (PrologVariable) structure.getElement(li);
      assertFalse(varSet.contains(currentVar));
      varSet.add(currentVar);
    }

    assertEquals(structure.getArity(), varSet.size());
  }

  @Test
  public void testVariablesAtSentenceBounds() {
    PrologStructure structure = (PrologStructure) parserFor("test(A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C).").next();

    final PrologVariable varA = (PrologVariable) structure.getElement(0);
    final PrologVariable varB = (PrologVariable) structure.getElement(1);
    final PrologVariable varC = (PrologVariable) structure.getElement(2);

    assertNotSame(varA, varB);
    assertNotSame(varA, varC);
    assertNotSame(varB, varC);

    assertEquals("A", varA.getText());
    assertEquals("B", varB.getText());
    assertEquals("C", varC.getText());

    for (int li = 3; li < structure.getArity(); ) {
      assertNotSame(varA, structure.getElement(li++));
      assertNotSame(varB, structure.getElement(li++));
      assertNotSame(varC, structure.getElement(li++));
    }
  }

  @Test
  public void testEndOfStream() {
    final PrologParser parser = parserFor("hello.world.");
    AbstractPrologTerm term = parser.next();

    assertEquals(PrologTermType.ATOM, term.getType());
    assertEquals("hello", term.getText());

    term = parser.next();
    assertEquals(PrologTermType.ATOM, term.getType());
    assertEquals("world", term.getText());

    assertThrows(NoSuchElementException.class, () -> parser.next());
  }

  private void checkWrongSentenceReadingWithPPE(final String readSentence, final int stringPosition) {
    assertEquals(stringPosition, assertThrows(PrologParserException.class, () -> parserFor(readSentence).next()).getStringPosition());
  }

  @Test
  public void testErrorListDefinitions() {
    checkWrongSentenceReadingWithPPE(" [,].", 3);
    checkWrongSentenceReadingWithPPE(" [|].", 2);
    checkWrongSentenceReadingWithPPE(" [345|].", 7);
    checkWrongSentenceReadingWithPPE(" [345|323|X].", 11);
    checkWrongSentenceReadingWithPPE(" [345|323|].", 10);
    checkWrongSentenceReadingWithPPE(" [|345].", 2);
    checkWrongSentenceReadingWithPPE(" [A|((((B|C))))].", 15);
    checkWrongSentenceReadingWithPPE(" [1,2,3.", 8);
    checkWrongSentenceReadingWithPPE(" 1,2,3].", 7);
  }

  private void checkParseAtomWithoutPPE(final String atomToBeChecked, final String expectedAtomText) {
    final AbstractPrologTerm atom = parserFor(atomToBeChecked + '.').next();
    assertEquals(PrologTermType.ATOM, atom.getType(), "Type: " + atom.getType());
    assertEquals(PrologAtom.class, atom.getClass(), "Class: " + atom.getClass());
    assertEquals(expectedAtomText, atom.getText(), "Text: " + atom.getText());
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
    AbstractPrologTerm atom = parserFor(atomToBeChecked + '.').next();
    assertEquals(PrologTermType.ATOM, atom.getType(), "Type: " + atom.getType());
    assertEquals(PrologIntegerNumber.class, atom.getClass(), "Class: " + atom.getClass());
    assertEquals(expectedNumber, ((PrologIntegerNumber) atom).getValue().longValue(), "Number: " + ((PrologIntegerNumber) atom).getValue().longValue());
    assertEquals(Long.toString(expectedNumber), atom.getText(), "Text: " + atom.getText());
  }

  @Test
  public void testParseInteger() {
    checkIntegerWithoutPPE("1", 1);
    checkIntegerWithoutPPE("1313", 1313);
    checkIntegerWithoutPPE("-0", 0);
    checkIntegerWithoutPPE("-97", -97);
    checkIntegerWithoutPPE("-97", -97);
    checkIntegerWithoutPPE(Long.toString(Long.MAX_VALUE), Long.MAX_VALUE);

    checkIntegerWithoutPPE(Long.toString(Long.MIN_VALUE), Long.MIN_VALUE);

    final AbstractPrologTerm val = parserFor("'298723987'.").next();
    assertEquals(PrologTermType.ATOM, val.getType());
    assertEquals(PrologAtom.class, val.getClass());
    assertEquals("298723987", val.getText());
  }

  private void checkFloatWithoutPPE(final String atomToBeChecked, final double expectedNumber) {
    AbstractPrologTerm atom = parserFor(atomToBeChecked + '.').next();
    assertEquals(PrologTermType.ATOM, atom.getType());
    assertEquals(PrologFloatNumber.class, atom.getClass());
    assertEquals(expectedNumber, ((PrologFloatNumber) atom).getValue().doubleValue(), Double.MIN_NORMAL, String.format("%e <> %e", expectedNumber, ((PrologFloatNumber) atom).getValue().doubleValue()));
    assertEquals(BigDecimal.valueOf(expectedNumber).toEngineeringString(), atom.getText());
  }

  @Test
  public void testParseFloat() {
    checkFloatWithoutPPE(new BigDecimal(Math.PI, PrologFloatNumber.MATH_CONTEXT).toEngineeringString(), Math.PI);
    checkFloatWithoutPPE("-0.0035", -0.0035d);
    checkFloatWithoutPPE("100.2", 100.2d);
    checkFloatWithoutPPE("2000.0", 2.0e+3d);

    final AbstractPrologTerm val = parserFor("298723987493287423423.00002342342300043324234324E+75.").next();
    assertEquals(PrologTermType.ATOM, val.getType());
    assertEquals(PrologFloatNumber.class, val.getClass());

    final AbstractPrologTerm valAtom = parserFor("2.0E.").next();
    assertEquals(PrologTermType.ATOM, valAtom.getType());
    assertEquals(PrologAtom.class, valAtom.getClass());
  }

  @Test
  public void testParseVariable() {
    AbstractPrologTerm var = parserFor("X.").next();
    assertEquals(PrologTermType.VAR, var.getType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("X", var.getText());

    var = parserFor("Result.").next();
    assertEquals(PrologTermType.VAR, var.getType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("Result", var.getText());

    var = parserFor("Object2.").next();
    assertEquals(PrologTermType.VAR, var.getType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("Object2", var.getText());

    var = parserFor("Participant_list.").next();
    assertEquals(PrologTermType.VAR, var.getType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("Participant_list", var.getText());

    var = parserFor("ShoppingList.").next();
    assertEquals(PrologTermType.VAR, var.getType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("ShoppingList", var.getText());

    var = parserFor("_x23.").next();
    assertEquals(PrologTermType.VAR, var.getType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("_x23", var.getText());

    var = parserFor("_23.").next();
    assertEquals(PrologTermType.VAR, var.getType());
    assertEquals(PrologVariable.class, var.getClass());
    assertEquals("_23", var.getText());
  }

  @Test
  public void testParseStructure() {
    PrologParser parser = parserFor("date(Day,may,2001).");
    final AbstractPrologTerm term = parser.next();

    verify(parser.getContext(), times(1)).processNewStructure(any(EdinburghPrologParser.class), any(PrologStructure.class));

    assertEquals(PrologTermType.STRUCT, term.getType());

    PrologStructure struct = (PrologStructure) term;
    assertEquals(PrologTermType.ATOM, struct.getFunctor().getType());
    assertEquals("date", struct.getFunctor().getText());
    assertEquals(3, struct.getArity());
    assertEquals(PrologTermType.VAR, struct.getElement(0).getType());
    assertEquals("Day", struct.getElement(0).getText());
    assertEquals(PrologTermType.ATOM, struct.getElement(1).getType());
    assertEquals("may", struct.getElement(1).getText());
    assertEquals(PrologTermType.ATOM, struct.getElement(2).getType());
    assertEquals(2001L,
        ((PrologIntegerNumber) struct.getElement(2)).getValue().longValue());
  }

  @Test
  public void testParseList() {
    PrologList list = (PrologList) parserFor("[].").next();
    assertEquals(PrologTermType.LIST, list.getType());
    assertEquals(PrologList.class, list.getClass());
    assertTrue((list).isNullList());

    list = (PrologList) parserFor("[1,2,3,4,5].").next();
    assertFalse(list.isNullList());
    assertEquals("[1, 2, 3, 4, 5]", list.toString());

    list = (PrologList) parserFor("[Hello|World].").next();
    assertFalse(list.isNullList());
    assertEquals("[Hello|World]", list.toString());
    assertEquals(PrologTermType.VAR, list.getHead().getType());
    assertEquals("Hello", list.getHead().getText());
    assertEquals(PrologTermType.VAR, list.getTail().getType());
    assertEquals("World", list.getTail().getText());

    list = (PrologList) parserFor("[a|[b,c]].").next();
    assertFalse(list.isNullList());
    assertEquals("['a', 'b', 'c']", list.toString());

    list = (PrologList) parserFor("[a,b|[c]].").next();
    assertFalse(list.isNullList());
    assertEquals("['a', 'b', 'c']", list.toString());

    list = (PrologList) parserFor("[a,b,c|[]].").next();
    assertFalse(list.isNullList());
    assertEquals("['a', 'b', 'c']", list.toString());
  }

  @Test
  public void testParseOperator() {
    PrologParser parser = parserFor("hello:-world.");
    PrologStructure struct = (PrologStructure) parser.next();

    verify(parser.getContext(), times(1)).processNewStructure(any(EdinburghPrologParser.class), any(PrologStructure.class));

    assertEquals(PrologStructure.class, struct.getClass());
    assertEquals(PrologTermType.OPERATOR, struct.getFunctor().getType());
    assertEquals(":-", struct.getFunctor().getText());
    assertEquals(2, struct.getArity());
    assertEquals(OpType.XFX,
        ((Operator) struct.getFunctor()).getOperatorType());
    assertEquals("hello", struct.getElement(0).getText());
    assertEquals("world", struct.getElement(1).getText());

    parser = parserFor(":-test.");
    struct = (PrologStructure) parser.next();

    verify(parser.getContext(), times(1)).processNewStructure(any(EdinburghPrologParser.class), any(PrologStructure.class));

    assertEquals(PrologStructure.class, struct.getClass());
    assertEquals(PrologTermType.OPERATOR, struct.getFunctor().getType());
    assertEquals(":-", struct.getFunctor().getText());
    assertEquals(1, struct.getArity());
    assertEquals(OpType.FX,
        ((Operator) struct.getFunctor()).getOperatorType());
    assertEquals("test", struct.getElement(0).getText());

    parser = parserFor("X is X+1.");
    struct = (PrologStructure) parser.next();

    verify(parser.getContext(), times(2)).processNewStructure(any(EdinburghPrologParser.class), any(PrologStructure.class));

    assertEquals(PrologStructure.class, struct.getClass());
    assertEquals(PrologTermType.OPERATOR, struct.getFunctor().getType());
    assertEquals("is", struct.getFunctor().getText());
    assertEquals(2, struct.getArity());
    assertEquals(OpType.XFX,
        ((Operator) struct.getFunctor()).getOperatorType());
    assertEquals("X", struct.getElement(0).getText());
    assertEquals(PrologTermType.STRUCT, struct.getElement(1).getType());
    assertEquals("+", ((PrologStructure) struct.getElement(1)).getFunctor().getText());
    assertEquals(OpType.YFX,
        ((Operator) ((PrologStructure) struct.getElement(1)).getFunctor()).getOperatorType());
    assertEquals(2, ((PrologStructure) struct.getElement(1)).getArity());
    assertEquals("X", ((PrologStructure) struct.getElement(1)).getElement(0).getText());
    assertEquals("1", ((PrologStructure) struct.getElement(1)).getElement(1).getText());
  }

  @Test
  public void testComments() {
    final PrologParser parser = parserFor("  %   zero line %%%% misc%%%% \n%first line\n%second line\n\nhello:-world.%test\n:-test.");
    AbstractPrologTerm term = parser.next();
    AbstractPrologTerm term2 = parser.next();

    assertEquals("'hello' :- 'world'", term.toString());
    assertEquals(":- 'test'", term2.toString());

    assertFalse(parser.hasNext());
  }


  @Test
  public void testSimilarOperatorInterpretation() {
    final Map<String, OperatorContainer> operators = new HashMap<>();
    final Operator firstOperator = Operator.makeOperator(100, OpType.FY, "++++++");
    final Operator secondOperator = Operator.makeOperator(100, OpType.FY, "+++");
    final Operator thirdOperator = Operator.makeOperator(100, OpType.FY, "++");

    operators.put(firstOperator.getText(), newOpCont(firstOperator));
    operators.put(secondOperator.getText(), newOpCont(secondOperator));
    operators.put(thirdOperator.getText(), newOpCont(thirdOperator));

    final StubContext contextStub = new StubContext(operators);
    final AbstractPrologTerm term = new EdinburghPrologParser(new StringReader("+++++++++++ 3."), contextStub).next();

    assertSame(firstOperator, ((PrologStructure) term).getFunctor());
    assertSame(secondOperator, ((PrologStructure) (((PrologStructure) term).getElement(0))).getFunctor());
    assertSame(thirdOperator, ((PrologStructure) ((PrologStructure) (((PrologStructure) term).getElement(0))).getElement(0)).getFunctor());

  }

  @Test
  public void testOperatorHierarchy() {
    final Map<String, OperatorContainer> operators = new HashMap<>();

    final StubContext contextStub = new StubContext(operators);
    final PrologParser parser = new EdinburghPrologParser(new StringReader(":-op(800,xfx,'<===>').:-op(700,xfy,'v').:-op(600,xfy,'&').:-op(500,fy,'~').~(A&B)<===> ~A v ~B."), contextStub);

    AbstractPrologTerm term = null;

    while (parser.hasNext()) {
      term = parser.next();

      if (term.getType() == PrologTermType.STRUCT) {
        final PrologStructure structure = (PrologStructure) term;
        if (structure.getArity() == 1 && structure.getFunctor().getText().equals(":-")) {
          final PrologStructure operatorstructure = (PrologStructure) structure.getElement(0);

          if (operatorstructure.getArity() == 3 && operatorstructure.getFunctor().getText().equals("op")) {
            final Operator newoperator = Operator.makeOperator(
                ((PrologIntegerNumber) operatorstructure.getElement(0)).getValue().intValue(),
                OpType.getForName(operatorstructure.getElement(1).getText()).get(),
                operatorstructure.getElement(2).getText());

            OperatorContainer container = operators.get(newoperator.getText());

            if (container == null) {
              container = newOpCont(newoperator);
              operators.put(newoperator.getText(), container);
            } else {
              container.addOp(newoperator);
            }

          } else {
            fail("Unsupported structure detected");
          }
        }
      }
    }

    assertEquals(PrologTermType.STRUCT, term.getType());
    assertEquals("<===>", term.getText());
    assertEquals(2, ((PrologStructure) term).getArity());
    assertEquals(800, term.getPrecedence());

    final PrologStructure leftBranch = (PrologStructure) ((PrologStructure) term).getElement(0);
    final PrologStructure rightBranch = (PrologStructure) ((PrologStructure) term).getElement(1);

    assertEquals(500, leftBranch.getPrecedence());
    assertEquals("~", leftBranch.getText());
    assertEquals(1, leftBranch.getArity());

    assertEquals(700, rightBranch.getPrecedence());
    assertEquals("v", rightBranch.getText());
    assertEquals(2, rightBranch.getArity());

    final PrologStructure leftBranch2 = (PrologStructure) leftBranch.getElement(0);
    final PrologStructure rightBranchL = (PrologStructure) rightBranch.getElement(0);
    final PrologStructure rightBranchR = (PrologStructure) rightBranch.getElement(1);

    assertEquals(600, leftBranch2.getPrecedence());
    assertEquals("&", leftBranch2.getText());
    assertEquals(2, leftBranch2.getArity());
    assertEquals("A", leftBranch2.getElement(0).getText());
    assertEquals("B", leftBranch2.getElement(1).getText());

    assertEquals(500, rightBranchL.getPrecedence());
    assertEquals("~", rightBranchL.getText());
    assertEquals(1, rightBranchL.getArity());
    assertEquals("A", rightBranchL.getElement(0).getText());

    assertEquals(500, rightBranchR.getPrecedence());
    assertEquals("~", rightBranchR.getText());
    assertEquals(1, rightBranchR.getArity());
    assertEquals("B", rightBranchR.getElement(0).getText());

    assertEquals("~ (A & B) <===> ~ A v ~ B", term.toString());
  }

  private void assertReadTerms(final int expected, final String resource) {
    try (Reader reader = new InputStreamReader(getClass().getResourceAsStream(resource), StandardCharsets.UTF_8)) {
      final PrologParser parser = new EdinburghPrologParser(reader, mock(ParserContext.class));
      assertEquals(expected, parser.stream().count());
    } catch (IOException ex) {
      ex.printStackTrace();
      fail("IOException");
    }
  }

  @Test
  public void testParseFileAsChannel() {
    assertReadTerms(26, "sec812.pro");
    assertReadTerms(25, "sec816.pro");
    assertReadTerms(32, "sec811.pro");
  }

  @Test
  public void testStreamPositionForTerms() {
    AbstractPrologTerm atom = parserFor("\n     'hello'.").next();
    assertEquals(6, atom.getStrPosition());
    assertEquals(2, atom.getLineNumber());

    atom = parserFor("\n     12345.").next();
    assertEquals(6, atom.getStrPosition());
    assertEquals(2, atom.getLineNumber());

    atom = parserFor("\n     [1,2,3,4,5].").next();
    assertEquals(6, atom.getStrPosition());
    assertEquals(2, atom.getLineNumber());

    final PrologList list = (PrologList) parserFor("\n\n\n\n   [   [1,2],3].").next();
    assertEquals(4, list.getStrPosition());
    assertEquals(5, list.getLineNumber());

    assertEquals(8, list.getHead().getStrPosition());
    assertEquals(5, list.getHead().getLineNumber());

    final PrologStructure mainterm = (PrologStructure) parserFor("  %\ncube (X,\'hello\',1000) :- \n Y is X * X * X.").next();

    // ':-'
    assertEquals(23, mainterm.getStrPosition());
    assertEquals(2, mainterm.getLineNumber());

    final PrologStructure leftPart = (PrologStructure) mainterm.getElement(0);
    // 'cube(X,Y)'
    assertEquals(1, leftPart.getStrPosition());
    assertEquals(2, leftPart.getLineNumber());
    // 'X'
    assertEquals(7, leftPart.getElement(0).getStrPosition());
    assertEquals(2, leftPart.getElement(0).getLineNumber());
    // 'hello'
    assertEquals(9, leftPart.getElement(1).getStrPosition());
    assertEquals(2, leftPart.getElement(1).getLineNumber());
    // 1000
    assertEquals(17, leftPart.getElement(2).getStrPosition());
    assertEquals(2, leftPart.getElement(2).getLineNumber());

    final PrologStructure rightPart = (PrologStructure) mainterm.getElement(1);
    // 'is'
    assertEquals(4, rightPart.getStrPosition());
    assertEquals(3, rightPart.getLineNumber());
    // 'Y'
    assertEquals(2, rightPart.getElement(0).getStrPosition());
    assertEquals(3, rightPart.getElement(0).getLineNumber());

    final PrologStructure structure = (PrologStructure) parserFor("test(\n1,  \n2, 4*2,  \n3,4,\n5,6*7,8).").next();
    // 4*2
    assertEquals(5, structure.getElement(2).getStrPosition());
    assertEquals(3, structure.getElement(2).getLineNumber());
    // 6*7
    assertEquals(4, structure.getElement(6).getStrPosition());
    assertEquals(5, structure.getElement(6).getLineNumber());
  }

  @Test
  public void testSingleOperatorAsAtom() {
    final PrologStructure structure = (PrologStructure) parserFor("not/stream.").next();
    assertEquals("/", structure.getFunctor().getText());
    assertEquals(PrologTermType.ATOM, structure.getElement(0).getType());
    assertEquals("not", structure.getElement(0).getText());
    assertEquals(PrologTermType.ATOM, structure.getElement(1).getType());
    assertEquals("stream", structure.getElement(1).getText());
  }

  @Test
  public void testRecognizingUserOperatorsWhichSimilarMetaOperators() {
    final Map<String, OperatorContainer> operators = new HashMap<>();
    operators.put("(((", newOpCont(Operator.makeOperator(1, OpType.FX, "(((")));
    operators.put("...", newOpCont(Operator.makeOperator(1200, OpType.XF, "...")));
    final StubContext stubContext = new StubContext(operators);

    final PrologStructure structure = (PrologStructure) new EdinburghPrologParser(new StringReader("(((hello...."), stubContext).next();
    assertEquals("...", structure.getFunctor().getText());
    assertEquals("(((", ((PrologStructure) structure.getElement(0)).getFunctor().getText());
    assertEquals("hello", ((PrologStructure) structure.getElement(0)).getElement(0).getText());
  }

  @Test
  public void testOperatorNameAsAtomicFunctor() {
    final PrologStructure structure = (PrologStructure) parserFor("'mod'(_,_,_,_).").next();
    assertEquals("mod", structure.getFunctor().getText());
    assertNotSame(structure.getFunctor().getType(), PrologTermType.OPERATOR);
    assertEquals(4, structure.getArity());
  }

  @Test
  public void testOperatorNameAsFunctor_EmptyBrackets() {
    final PrologParserException ex = assertThrows(PrologParserException.class, () -> parserFor("+().").next());
    assertEquals(2, ex.getStringPosition());
    assertEquals(1, ex.getLineNumber());
  }

  @Test
  public void testAtomAsFunctor_EmptyBrackets() {
    final PrologParserException ex = assertThrows(PrologParserException.class, () -> parserFor("'hello'().").next());
    assertEquals(8, ex.getStringPosition());
    assertEquals(1, ex.getLineNumber());
  }

  @Test
  public void testAtomInSequenceWithNameSimilarOperator() {
    final PrologStructure structure = (PrologStructure) parserFor("functor(1,2,3,mod,5,6).").next();
    assertEquals(6, structure.getArity());
    assertEquals("1", structure.getElement(0).getText());
    assertEquals("2", structure.getElement(1).getText());
    assertEquals("3", structure.getElement(2).getText());
    assertEquals("mod", structure.getElement(3).getText());
    assertEquals("5", structure.getElement(4).getText());
    assertEquals("6", structure.getElement(5).getText());
  }

  @Test
  @Disabled
  public void testVeryLongList() {
    final int ELEMENTS = 1000000;

    final StringBuilder buffer = new StringBuilder();

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

    PrologList list = (PrologList) parserFor(buffer.toString()).next();

    for (int i = 0; i < ELEMENTS; i++) {
      final PrologIntegerNumber head = (PrologIntegerNumber) list.getHead();
      assertEquals(i, head.getValue().intValue());
      list = (PrologList) list.getTail();
    }

    assertTrue(list.isNullList());
  }

  @Test
  public void testHelloWorld() {
    final PrologStructure rule = (PrologStructure) parserFor("hello :- world,!.").next();
    final PrologStructure and = (PrologStructure) rule.getElement(1);
    assertEquals("hello world!", rule.getElement(0).getText() + ' ' + and.getElement(0).getText() + and.getElement(1).getText());
  }

  @Test
  @Disabled
  public void testVeryLongStructure() {
    final int ELEMENTS = 1000000;

    final StringBuilder buffer = new StringBuilder();
    buffer.append("test(");
    boolean nonfirst = false;
    for (int i = 0; i < ELEMENTS; i++) {
      if (nonfirst) {
        buffer.append(',');
      } else {
        nonfirst = true;
      }
      buffer.append(i);
    }
    buffer.append(").");

    PrologStructure struct = (PrologStructure) parserFor(buffer.toString()).next();

    assertEquals(ELEMENTS, struct.getArity());
    assertEquals("test", struct.getFunctor().getText());
    for (int i = 0; i < ELEMENTS; i++) {
      assertEquals(i, ((PrologIntegerNumber) struct.getElement(i)).getValue().intValue());
    }
  }

  private static class StubContext implements ParserContext {

    private final Map<String, OperatorContainer> operators;

    public StubContext(final Map<String, OperatorContainer> operators) {
      this.operators = operators;
    }

    @Override
    public boolean hasOperatorStartsWith(final PrologParser source,
                                         final String operatorNameStartSubstring) {
      for (final String string : operators.keySet()) {
        if (string.startsWith(operatorNameStartSubstring)) {
          return true;
        }
      }

      return false;
    }

    @Override
    public OperatorContainer findOperatorForName(final PrologParser source,
                                                 final String operatorName) {
      return operators.get(operatorName);
    }

    @Override
    public boolean hasZeroArityPredicate(final PrologParser source, final String predicateName) {
      return false;
    }

    @Override
    public void processNewStructure(final PrologParser source, final PrologStructure structure) {
    }
  }

}
