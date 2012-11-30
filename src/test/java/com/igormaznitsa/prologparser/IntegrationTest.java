package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.operators.OperatorType;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloatNumber;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.terms.PrologVariable;
import com.igormaznitsa.prologparser.utils.StringUtils;
import java.math.BigDecimal;
import java.nio.channels.Channels;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import static org.mockito.Mockito.*;

public class IntegrationTest extends AbstractPrologParserTest {

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
    };
    final ParserContext mock = mock(ParserContext.class);
    final PrologParser parser = new PrologParser(mock);

    @Before
    public void onStartUp() {
        reset(mock);
    }

    @Test
    public void testParseStringWithSpecialChars() throws Exception {
        AbstractPrologTerm term = parser.nextSentence("'\u0008Hello\\\nWorld\u0021\\r'.'\\xFFAnother String\u0007'.");

        assertEquals(PrologTermType.ATOM, term.getType());
        assertEquals("\\bHello\\nWorld!\\r",
                StringUtils.escapeString(term.getText()));

        term = parser.nextSentence();
        assertNotNull(term);
        assertEquals("\u00FFAnother String\u0007", term.getText());

    }

    @Test
    public void testVariableMustBeNotEqualAtSentenceBounds() throws Exception {
        PrologStructure structure = (PrologStructure) parser.nextSentence("test(A,B,C,A,B,C,A,B,C,A,B,C,_,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,_,_).");

        final Set<PrologVariable> varSet = new HashSet<PrologVariable>();
        for (int li = 0; li < structure.getArity(); li++) {
            final PrologVariable currentVar = (PrologVariable) structure.getElement(li);
            assertFalse(varSet.contains(currentVar));
            varSet.add(currentVar);
        }

        assertEquals(structure.getArity(), varSet.size());
    }

    @Test
    public void testVariablesAtSentenceBounds() throws Exception {
        PrologStructure structure = (PrologStructure) parser.nextSentence("test(A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C).");

        final PrologVariable varA = (PrologVariable) structure.getElement(0);
        final PrologVariable varB = (PrologVariable) structure.getElement(1);
        final PrologVariable varC = (PrologVariable) structure.getElement(2);

        assertNotSame(varA, varB);
        assertNotSame(varA, varC);
        assertNotSame(varB, varC);

        assertEquals("A", varA.getText());
        assertEquals("B", varB.getText());
        assertEquals("C", varC.getText());

        for (int li = 3; li < structure.getArity();) {
            assertNotSame(varA, ((PrologVariable) structure.getElement(li++)));
            assertNotSame(varB, ((PrologVariable) structure.getElement(li++)));
            assertNotSame(varC, ((PrologVariable) structure.getElement(li++)));
        }
    }

    @Test
    public void testEndOfStream() throws Exception {
        AbstractPrologTerm term = parser.nextSentence("hello.world.");

        assertEquals(PrologTermType.ATOM, term.getType());
        assertEquals("hello", term.getText());

        term = parser.nextSentence();
        assertEquals(PrologTermType.ATOM, term.getType());
        assertEquals("world", term.getText());

        term = parser.nextSentence();
        assertNull(term);
    }

    private void checkWrongSentenceReadingWithPPE(final String readSentence, final int stringPosition)
            throws Exception {
        try {
            parser.nextSentence(readSentence);
            fail("Must throw PPE");
        } catch (PrologParserException ex) {
            //ex.printStackTrace();
            assertEquals(ex.toString(), stringPosition, ex.getStringPosition());
        }
    }

    @Test
    public void testErrorListDefinitions() throws Exception {
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

    private void checkParseAtomWithoutPPE(final String atomToBeChecked, final String expectedAtomText) throws Exception {
        AbstractPrologTerm atom = parser.nextSentence(atomToBeChecked + '.');
        assertEquals(PrologTermType.ATOM, atom.getType());
        assertEquals(PrologAtom.class, atom.getClass());
        assertEquals(expectedAtomText, atom.getText());
    }

    @Test
    public void testParseAtom() throws Exception {
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

    private void checkIntegerWithoutPPE(final String atomToBeChecked, final long expectedNumber) throws Exception {
        AbstractPrologTerm atom = parser.nextSentence(atomToBeChecked + '.');
        assertEquals(PrologTermType.ATOM, atom.getType());
        assertEquals(PrologIntegerNumber.class, atom.getClass());
        assertEquals(expectedNumber, ((PrologIntegerNumber) atom).getValue().longValue());
        assertEquals(Long.toString(expectedNumber), atom.getText());
    }

    @Test
    public void testParseInteger() throws Exception {
        checkIntegerWithoutPPE("1", 1);
        checkIntegerWithoutPPE("1313", 1313);
        checkIntegerWithoutPPE("-0", 0);
        checkIntegerWithoutPPE("-97", -97);
        checkIntegerWithoutPPE("-97", -97);
        checkIntegerWithoutPPE(Long.toString(Long.MAX_VALUE), Long.MAX_VALUE);

        checkIntegerWithoutPPE(Long.toString(Long.MIN_VALUE), Long.MIN_VALUE);

        final AbstractPrologTerm val = parser.nextSentence("'298723987'.");
        assertEquals(PrologTermType.ATOM, val.getType());
        assertEquals(PrologAtom.class, val.getClass());
        assertEquals("298723987", val.getText());
    }

    private void checkFloatWithoutPPE(final String atomToBeChecked, final double expectedNumber) throws Exception {
        AbstractPrologTerm atom = parser.nextSentence(atomToBeChecked + '.');
        assertEquals(PrologTermType.ATOM, atom.getType());
        assertEquals(PrologFloatNumber.class, atom.getClass());
        assertEquals(expectedNumber, ((PrologFloatNumber) atom).getValue().doubleValue(), 0d);
        assertEquals(BigDecimal.valueOf(expectedNumber).toEngineeringString(), atom.getText());
    }

    @Test
    public void testParseFloat() throws Exception {
        checkFloatWithoutPPE(new BigDecimal(Math.PI, PrologFloatNumber.MATH_CONTEXT).toEngineeringString(), Math.PI);
        checkFloatWithoutPPE("-0.0035", -0.0035d);
        checkFloatWithoutPPE("100.2", 100.2d);
        checkFloatWithoutPPE("2000.0", 2.0e+3d);

        final AbstractPrologTerm val = parser.nextSentence("298723987493287423423.00002342342300043324234324E+75.");
        assertEquals(PrologTermType.ATOM, val.getType());
        assertEquals(PrologFloatNumber.class, val.getClass());

        final AbstractPrologTerm valAtom = parser.nextSentence("2.0E.");
        assertEquals(PrologTermType.ATOM, valAtom.getType());
        assertEquals(PrologAtom.class, valAtom.getClass());
    }

    @Test
    public void testParseVariable() throws Exception {
        AbstractPrologTerm var = parser.nextSentence("X.");
        assertEquals(PrologTermType.VAR, var.getType());
        assertEquals(PrologVariable.class, var.getClass());
        assertEquals("X", var.getText());

        var = parser.nextSentence("Result.");
        assertEquals(PrologTermType.VAR, var.getType());
        assertEquals(PrologVariable.class, var.getClass());
        assertEquals("Result", var.getText());

        var = parser.nextSentence("Object2.");
        assertEquals(PrologTermType.VAR, var.getType());
        assertEquals(PrologVariable.class, var.getClass());
        assertEquals("Object2", var.getText());

        var = parser.nextSentence("Participant_list.");
        assertEquals(PrologTermType.VAR, var.getType());
        assertEquals(PrologVariable.class, var.getClass());
        assertEquals("Participant_list", var.getText());

        var = parser.nextSentence("ShoppingList.");
        assertEquals(PrologTermType.VAR, var.getType());
        assertEquals(PrologVariable.class, var.getClass());
        assertEquals("ShoppingList", var.getText());

        var = parser.nextSentence("_x23.");
        assertEquals(PrologTermType.VAR, var.getType());
        assertEquals(PrologVariable.class, var.getClass());
        assertEquals("_x23", var.getText());

        var = parser.nextSentence("_23.");
        assertEquals(PrologTermType.VAR, var.getType());
        assertEquals(PrologVariable.class, var.getClass());
        assertEquals("_23", var.getText());
    }

    @Test
    public void testParseStructure() throws Exception {
        final AbstractPrologTerm term = parser.nextSentence("date(Day,may,2001).");

        verify(mock, times(1)).processNewStructure(any(PrologParser.class), any(PrologStructure.class));

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
    public void testParseList() throws Exception {
        PrologList list = (PrologList) parser.nextSentence("[].");
        assertEquals(PrologTermType.LIST, list.getType());
        assertEquals(PrologList.class, list.getClass());
        assertTrue((list).isNullList());

        list = (PrologList) parser.nextSentence("[1,2,3,4,5].");
        assertFalse(list.isNullList());
        assertEquals("[1, 2, 3, 4, 5]", list.toString());

        list = (PrologList) parser.nextSentence("[Hello|World].");
        assertFalse(list.isNullList());
        assertEquals("[Hello|World]", list.toString());
        assertEquals(PrologTermType.VAR, list.getHead().getType());
        assertEquals("Hello", list.getHead().getText());
        assertEquals(PrologTermType.VAR, list.getTail().getType());
        assertEquals("World", list.getTail().getText());

        list = (PrologList) parser.nextSentence("[a|[b,c]].");
        assertFalse(list.isNullList());
        assertEquals("['a', 'b', 'c']", list.toString());

        list = (PrologList) parser.nextSentence("[a,b|[c]].");
        assertFalse(list.isNullList());
        assertEquals("['a', 'b', 'c']", list.toString());

        list = (PrologList) parser.nextSentence("[a,b,c|[]].");
        assertFalse(list.isNullList());
        assertEquals("['a', 'b', 'c']", list.toString());
    }

    @Test
    public void testParseOperator() throws Exception {
        PrologStructure struct = (PrologStructure) parser.nextSentence("hello:-world.");

        verify(mock, times(1)).processNewStructure(any(PrologParser.class), any(PrologStructure.class));

        assertEquals(PrologStructure.class, struct.getClass());
        assertEquals(PrologTermType.OPERATOR, struct.getFunctor().getType());
        assertEquals(":-", struct.getFunctor().getText());
        assertEquals(2, struct.getArity());
        assertEquals(OperatorType.XFX,
                ((Operator) struct.getFunctor()).getOperatorType());
        assertEquals("hello", struct.getElement(0).getText());
        assertEquals("world", struct.getElement(1).getText());

        reset(mock);
        struct = (PrologStructure) parser.nextSentence(":-test.");

        verify(mock, times(1)).processNewStructure(any(PrologParser.class), any(PrologStructure.class));

        assertEquals(PrologStructure.class, struct.getClass());
        assertEquals(PrologTermType.OPERATOR, struct.getFunctor().getType());
        assertEquals(":-", struct.getFunctor().getText());
        assertEquals(1, struct.getArity());
        assertEquals(OperatorType.FX,
                ((Operator) struct.getFunctor()).getOperatorType());
        assertEquals("test", struct.getElement(0).getText());

        reset(mock);
        struct = (PrologStructure) parser.nextSentence("X is X+1.");

        verify(mock, times(2)).processNewStructure(any(PrologParser.class), any(PrologStructure.class));

        assertEquals(PrologStructure.class, struct.getClass());
        assertEquals(PrologTermType.OPERATOR, struct.getFunctor().getType());
        assertEquals("is", struct.getFunctor().getText());
        assertEquals(2, struct.getArity());
        assertEquals(OperatorType.XFX,
                ((Operator) struct.getFunctor()).getOperatorType());
        assertEquals("X", struct.getElement(0).getText());
        assertEquals(PrologTermType.STRUCT, struct.getElement(1).getType());
        assertEquals("+", ((PrologStructure) struct.getElement(1)).getFunctor().getText());
        assertEquals(OperatorType.YFX,
                ((Operator) ((PrologStructure) struct.getElement(1)).getFunctor()).getOperatorType());
        assertEquals(2, ((PrologStructure) struct.getElement(1)).getArity());
        assertEquals("X", ((PrologStructure) struct.getElement(1)).getElement(0).getText());
        assertEquals("1", ((PrologStructure) struct.getElement(1)).getElement(1).getText());
    }

    @Test
    public void testComments() throws Exception {
        AbstractPrologTerm term = parser.nextSentence("  %   zero line %%%% misc%%%% \n%first line\n%second line\n\nhello:-world.%test\n:-test.");
        AbstractPrologTerm term2 = parser.nextSentence();

        assertEquals("'hello' :- 'world'", term.toString());
        assertEquals(":- 'test'", term2.toString());

        assertNull(parser.nextSentence());

    }

    @Test
    public void testParseFileAsStream() throws Exception {
        final PrologCharDataSource reader = new PrologCharDataSource(getClass().getResourceAsStream("sec811.pro"));
        try {
            AbstractPrologTerm term = parser.nextSentence(reader);
            assertNotNull(term);

            int index = 1;
            while (true) {
                try {
                    term = parser.nextSentence();
                    if (term == null) {
                        break;
                    }
                    index++;
                } catch (PrologParserException ex) {
                    fail("Prolog parser exception at " + ex.getLineNumber()
                            + ":" + ex.getStringPosition());
                }
            }
            assertEquals(32, index);
        } finally {
            reader.close();
        }
    }

    @Test
    public void testSimilarOperatorInterpretation() throws Exception {
        final Map<String, OperatorContainer> operators = new HashMap<String, OperatorContainer>();
        final Operator firstOperator = Operator.makeOperator(100, OperatorType.FY, "++++++");
        final Operator secondOperator = Operator.makeOperator(100, OperatorType.FY, "+++");
        final Operator thirdOperator = Operator.makeOperator(100, OperatorType.FY, "++");

        operators.put(firstOperator.getText(), new OperatorContainer(firstOperator));
        operators.put(secondOperator.getText(), new OperatorContainer(secondOperator));
        operators.put(thirdOperator.getText(), new OperatorContainer(thirdOperator));

        final StubContext contextStub = new StubContext(operators);
        final PrologParser parser = new PrologParser(contextStub);
        final AbstractPrologTerm term = parser.nextSentence("+++++++++++ 3.");

        assertSame(firstOperator, ((PrologStructure) term).getFunctor());
        assertSame(secondOperator, ((PrologStructure) (((PrologStructure) term).getElement(0))).getFunctor());
        assertSame(thirdOperator, ((PrologStructure) ((PrologStructure) (((PrologStructure) term).getElement(0))).getElement(0)).getFunctor());

    }

    @Test
    public void testOperatorHierarchy() throws Exception {
        final Map<String, OperatorContainer> operators = new HashMap<String, OperatorContainer>();

        final StubContext contextStub = new StubContext(operators);

        final PrologParser parser = new PrologParser(contextStub);

        AbstractPrologTerm term = parser.nextSentence(":-op(800,xfx,'<===>').:-op(700,xfy,'v').:-op(600,xfy,'&').:-op(500,fy,'~').~(A&B)<===> ~A v ~B.");
        AbstractPrologTerm lastterm = null;
        while (term != null) {
            lastterm = term;
            if (term.getType() == PrologTermType.STRUCT) {
                final PrologStructure structure = (PrologStructure) term;
                if (structure.getArity() == 1
                        && structure.getFunctor().getText().equals(":-")) {
                    final PrologStructure operatorstructure = (PrologStructure) structure.getElement(0);
                    if (operatorstructure.getArity() == 3
                            && operatorstructure.getFunctor().getText().equals("op")) {
                        final Operator newoperator = Operator.makeOperator(
                                ((PrologIntegerNumber) operatorstructure.getElement(0)).getValue().intValue(),
                                OperatorType.getForName(operatorstructure.getElement(1).getText()),
                                operatorstructure.getElement(2).getText());

                        OperatorContainer container = operators.get(newoperator.getText());
                        if (container == null) {
                            container = new OperatorContainer(newoperator);
                            operators.put(newoperator.getText(), container);
                        } else {
                            container.addOperator(newoperator);
                        }
                    } else {
                        throw new RuntimeException(
                                "Unsupported structure detected");
                    }
                }
            }
            term = parser.nextSentence();
        }

        assertNotNull(lastterm);
        assertEquals(PrologTermType.STRUCT, lastterm.getType());
        assertEquals("<===>", lastterm.getText());
        assertEquals(2, ((PrologStructure) lastterm).getArity());
        assertEquals(800, lastterm.getPriority());

        final PrologStructure leftBranch = (PrologStructure) ((PrologStructure) lastterm).getElement(0);
        final PrologStructure rightBranch = (PrologStructure) ((PrologStructure) lastterm).getElement(1);

        assertEquals(500, leftBranch.getPriority());
        assertEquals("~", leftBranch.getText());
        assertEquals(1, leftBranch.getArity());

        assertEquals(700, rightBranch.getPriority());
        assertEquals("v", rightBranch.getText());
        assertEquals(2, rightBranch.getArity());

        final PrologStructure leftBranch2 = (PrologStructure) leftBranch.getElement(0);
        final PrologStructure rightBranchL = (PrologStructure) rightBranch.getElement(0);
        final PrologStructure rightBranchR = (PrologStructure) rightBranch.getElement(1);

        assertEquals(600, leftBranch2.getPriority());
        assertEquals("&", leftBranch2.getText());
        assertEquals(2, leftBranch2.getArity());
        assertEquals("A", leftBranch2.getElement(0).getText());
        assertEquals("B", leftBranch2.getElement(1).getText());

        assertEquals(500, rightBranchL.getPriority());
        assertEquals("~", rightBranchL.getText());
        assertEquals(1, rightBranchL.getArity());
        assertEquals("A", rightBranchL.getElement(0).getText());

        assertEquals(500, rightBranchR.getPriority());
        assertEquals("~", rightBranchR.getText());
        assertEquals(1, rightBranchR.getArity());
        assertEquals("B", rightBranchR.getElement(0).getText());

        assertEquals("~ (A & B) <===> ~ A v ~ B", lastterm.toString());
    }

    @Test
    public void testParseFileAsChannel() throws Exception {
        final PrologCharDataSource reader = new PrologCharDataSource(
                Channels.newChannel(getClass().getResourceAsStream("sec812.pro")));
        AbstractPrologTerm term = null;
        int index = 0;
        try {
            term = parser.nextSentence(reader);
            assertNotNull(term);
            index++;

            while (true) {
                try {
                    term = parser.nextSentence();
                    if (term == null) {
                        break;
                    }
                    index++;
                } catch (PrologParserException ex) {
                    fail("Prolog parser exception at " + ex.getLineNumber()
                            + ":" + ex.getStringPosition());
                }
            }
            assertEquals(26, index);
        } finally {
            reader.close();
        }

        final PrologCharDataSource reader2 = new PrologCharDataSource(
                Channels.newChannel(getClass().getResourceAsStream("sec816.pro")));
        try {
            term = parser.nextSentence(reader2);
            assertNotNull(term);

            index = 1;
            while (true) {
                try {
                    term = parser.nextSentence();
                    if (term == null) {
                        break;
                    }
                    index++;
                } catch (PrologParserException ex) {
                    ex.printStackTrace();
                    fail("Prolog parser exception at " + ex.getLineNumber()
                            + ":" + ex.getStringPosition());
                }
            }
            assertEquals(25, index);
        } finally {
            reader2.close();
        }

    }

    @Test
    public void testStreamPositionForTerms() throws Exception {
        AbstractPrologTerm atom = parser.nextSentence("\n     'hello'.");
        assertEquals(6, atom.getStrPosition());
        assertEquals(2, atom.getLineNumber());

        atom = parser.nextSentence("\n     12345.");
        assertEquals(6, atom.getStrPosition());
        assertEquals(2, atom.getLineNumber());

        atom = parser.nextSentence("\n     [1,2,3,4,5].");
        assertEquals(6, atom.getStrPosition());
        assertEquals(2, atom.getLineNumber());

        final PrologList list = (PrologList) parser.nextSentence("\n\n\n\n   [   [1,2],3].");
        assertEquals(4, list.getStrPosition());
        assertEquals(5, list.getLineNumber());

        assertEquals(8, list.getHead().getStrPosition());
        assertEquals(5, list.getHead().getLineNumber());

        final PrologStructure mainterm = (PrologStructure) parser.nextSentence("  %\ncube (X,\'hello\',1000) :- \n Y is X * X * X.");

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

        final PrologStructure structure = (PrologStructure) parser.nextSentence("test(\n1,  \n2, 4*2,  \n3,4,\n5,6*7,8).");
        // 4*2
        assertEquals(5, structure.getElement(2).getStrPosition());
        assertEquals(3, structure.getElement(2).getLineNumber());
        // 6*7
        assertEquals(4, structure.getElement(6).getStrPosition());
        assertEquals(5, structure.getElement(6).getLineNumber());
    }

    @Test
    public void testSingleOperatorAsAtom() throws Exception {
        final PrologStructure structure = (PrologStructure) parser.nextSentence("not/stream.");
        assertEquals("/", structure.getFunctor().getText());
        assertEquals("It must be an atom", PrologTermType.ATOM, structure.getElement(0).getType());
        assertEquals("not", structure.getElement(0).getText());
        assertEquals(PrologTermType.ATOM, structure.getElement(1).getType());
        assertEquals("stream", structure.getElement(1).getText());
    }

    @Test
    public void testRecognizingUserOperatorsWhichSimilarMetaOperators() throws Exception {
        final Map<String, OperatorContainer> operators = new HashMap<String, OperatorContainer>();
        operators.put("(((", new OperatorContainer(Operator.makeOperator(1, OperatorType.FX, "(((")));
        operators.put("...", new OperatorContainer(Operator.makeOperator(1200, OperatorType.XF, "...")));
        final StubContext stubContext = new StubContext(operators);

        final PrologStructure structure = (PrologStructure) new PrologParser(stubContext).nextSentence("(((hello....");
        assertEquals("Must be '...' operator", "...", structure.getFunctor().getText());
        assertEquals("Must be '(((' operator", "(((", ((PrologStructure) structure.getElement(0)).getFunctor().getText());
        assertEquals("Must be 'hello' atom", "hello", ((PrologStructure) structure.getElement(0)).getElement(0).getText());
    }

    @Test
    public void testOperatorNameAsAtomicFunctor() throws Exception {
        final PrologStructure structure = (PrologStructure) new PrologParser(null).nextSentence("'mod'(_,_,_,_).");
        assertEquals("Must be mod", "mod", structure.getFunctor().getText());
        assertTrue("Must not be an operator", structure.getFunctor().getType() != PrologTermType.OPERATOR);
        assertEquals("Arity must be 4", 4, structure.getArity());
    }

    @Test
    public void testOperatorNameAsFunctor_EmptyBrackets() throws Exception {
        try {
            new PrologParser(null).nextSentence("+().");
            fail("Must throw PPE");
        } catch (PrologParserException ex) {
            assertEquals("Must be the 2 pos", 2, ex.getStringPosition());
            assertEquals("Must be the 1 line", 1, ex.getLineNumber());
        }
    }

    @Test
    public void testAtomAsFunctor_EmptyBrackets() throws Exception {
        try {
            new PrologParser(null).nextSentence("'hello'().");
            fail("Must throw PPE");
        } catch (PrologParserException ex) {
            assertEquals("Must be the 8 pos", 8, ex.getStringPosition());
            assertEquals("Must be the 1 line", 1, ex.getLineNumber());
        }
    }

}
