package com.igormaznitsa.prologparser;

import static org.junit.Assert.*;

import java.nio.channels.Channels;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

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

public class IntegrationTest extends AbstractPrologParserTest {
	final ParserContext mock = Mockito.mock(ParserContext.class);
	final PrologParser parser = new PrologParser(mock);

	@Before
	public void onStartUp() {
		Mockito.reset(mock);
	}

	@Test
	public void testParseStringWithSpecialChars() throws Exception {
		AbstractPrologTerm term = parser
				.nextSentence("'\u0008Hello\\\nWorld\u0021\\r'.'\\xFFAnother String\u0007'.");

		assertEquals(PrologTermType.ATOM, term.getType());
		assertEquals("\\bHello\\nWorld!\\r",
				StringUtils.escapeString(term.getText()));

		term = parser.nextSentence();
		assertNotNull(term);
		assertEquals("\u00FFAnother String\u0007", term.getText());

	}

	@Test
	public void testVariableMustBeEqualAtSentenceBounds() throws Exception {
		PrologStructure structure = (PrologStructure) parser
				.nextSentence("test(A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C).");
		PrologVariable varA = (PrologVariable) structure.getElement(0);
		PrologVariable varB = (PrologVariable) structure.getElement(1);
		PrologVariable varC = (PrologVariable) structure.getElement(2);

		assertNotNull(varA);
		assertNotNull(varB);
		assertNotNull(varC);

		assertEquals("A", varA.getText());
		assertEquals("B", varB.getText());
		assertEquals("C", varC.getText());

		assertNotSame(varA, varB);
		assertNotSame(varA, varC);
		assertNotSame(varB, varC);

		for (int li = 0; li < structure.getArity(); li += 3) {
			assertSame(varA, structure.getElement(li));
			assertSame(varB, structure.getElement(li + 1));
			assertSame(varC, structure.getElement(li + 2));
		}

		PrologList list = (PrologList) parser
				.nextSentence("[A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C,A,B,C].");
		varA = (PrologVariable) list.getHead();
		varB = (PrologVariable) ((PrologList) list.getTail()).getHead();
		varC = (PrologVariable) ((PrologList) ((PrologList) list.getTail())
				.getTail()).getHead();

		assertNotNull(varA);
		assertNotNull(varB);
		assertNotNull(varC);

		assertEquals("A", varA.getText());
		assertEquals("B", varB.getText());
		assertEquals("C", varC.getText());

		assertNotSame(varA, varB);
		assertNotSame(varA, varC);
		assertNotSame(varB, varC);

		final PrologVariable[] checkarray = new PrologVariable[] { varA, varB,
				varC };
		int index = 0;
		while (!list.isNullList()) {
			assertSame(checkarray[index % 3], list.getHead());
			index++;
			list = (PrologList) list.getTail();
		}
		assertEquals(21, index);

		structure = (PrologStructure) parser
				.nextSentence("test(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).");
		final Set<PrologVariable> vars = new HashSet<PrologVariable>();
		for (int i = 0; i < structure.getArity(); i++) {
			final PrologVariable var = (PrologVariable) structure.getElement(i);
			assertTrue(var.isAnonymous());
			assertEquals("_", var.getText());
			assertFalse(vars.contains(var));
			vars.add(var);
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

	private void checkWrongSentenceReadingWithPPE(final String readSentence)
			throws Exception {
		try {
			parser.nextSentence("[|].");
			fail("Must throw PPE");
		} catch (PrologParserException ex) {
		}
	}

	@Test
	public void testErrorListDefinitions() throws Exception {
		checkWrongSentenceReadingWithPPE("[,].");
		checkWrongSentenceReadingWithPPE("[|].");
		checkWrongSentenceReadingWithPPE("[345|].");
		checkWrongSentenceReadingWithPPE("[345|323|X].");
		checkWrongSentenceReadingWithPPE("[345|323|].");
		checkWrongSentenceReadingWithPPE("[|345].");
		checkWrongSentenceReadingWithPPE("[A|((((B|C))))].");
		checkWrongSentenceReadingWithPPE("[1,2,3.");
		checkWrongSentenceReadingWithPPE("1,2,3].");
	}

	private void checkParseAtomWithoutPPE(final String atomToBeChecked, final String expectedAtomText) throws Exception {
		AbstractPrologTerm atom = parser.nextSentence(atomToBeChecked+'.');
		assertEquals(PrologTermType.ATOM, atom.getType());
		assertEquals(PrologAtom.class, atom.getClass());
		assertEquals(expectedAtomText, atom.getText());
	}
	
	@Test
	public void testParseAtom() throws Exception {
		checkParseAtomWithoutPPE("a","a");
		checkParseAtomWithoutPPE("test012","test012");
		checkParseAtomWithoutPPE("x______y","x______y");
		checkParseAtomWithoutPPE("alpha_beta_procedure","alpha_beta_procedure");
		// test of non-latin chars, "hello" in russian
		checkParseAtomWithoutPPE("привет","привет");
		checkParseAtomWithoutPPE("miss_Jones","miss_Jones");
		checkParseAtomWithoutPPE("\'Jones\'","Jones");
		checkParseAtomWithoutPPE("\'\'","");
		checkParseAtomWithoutPPE("x_","x_");
	}

	
	private void checkIntegerWithoutPPE(final String atomToBeChecked, final long expectedNumber) throws Exception {
		AbstractPrologTerm atom = parser.nextSentence(atomToBeChecked+'.');
		assertEquals(PrologTermType.ATOM, atom.getType());
		assertEquals(PrologIntegerNumber.class, atom.getClass());
		assertEquals(expectedNumber, ((PrologIntegerNumber) atom).getValue());
		assertEquals(Long.toString(expectedNumber), atom.getText());
	}
	
	@Test
	public void testParseInteger() throws Exception {
		checkIntegerWithoutPPE("1", 1);
		checkIntegerWithoutPPE("1313",1313);
		checkIntegerWithoutPPE("-0",0);
		checkIntegerWithoutPPE("-97",-97);
		checkIntegerWithoutPPE("-97",-97);
		checkIntegerWithoutPPE(Long.toString(Long.MAX_VALUE),Long.MAX_VALUE);
		
		// because we use minus as an operator, the min value will be not Long.MIN_VALUE but Long.MIN_VALUE+1 
		checkIntegerWithoutPPE(Long.toString(Long.MIN_VALUE+1),Long.MIN_VALUE+1);
		
		final AbstractPrologTerm val = parser.nextSentence("'298723987'.");
		assertEquals(PrologTermType.ATOM, val.getType());
		assertEquals(PrologAtom.class, val.getClass());
		assertEquals("298723987", val.getText());
	}

	private void checkFloatWithoutPPE(final String atomToBeChecked, final double expectedNumber) throws Exception {
		AbstractPrologTerm atom = parser.nextSentence(atomToBeChecked+'.');
		assertEquals(PrologTermType.ATOM, atom.getType());
		assertEquals(PrologFloatNumber.class, atom.getClass());
		assertEquals(expectedNumber, ((PrologFloatNumber) atom).getValue(),0d);
		assertEquals(Double.toString(expectedNumber), atom.getText());
	}
	
	@Test
	public void testParseFloat() throws Exception {
		checkFloatWithoutPPE(Double.toString(Math.PI), Math.PI);
		checkFloatWithoutPPE("-0.0035", -0.0035d);
		checkFloatWithoutPPE("100.2", 100.2d);
		checkFloatWithoutPPE("2.0E+3", 2.0e+3d);

		checkFloatWithoutPPE(Double.toString(Double.MIN_NORMAL), Double.MIN_NORMAL);
		checkFloatWithoutPPE(Double.toString(Double.MIN_VALUE), Double.MIN_VALUE);
		checkFloatWithoutPPE(Double.toString(Double.MIN_EXPONENT), Double.MIN_EXPONENT);
		checkFloatWithoutPPE(Double.toString(Double.MAX_VALUE), Double.MAX_VALUE);
		checkFloatWithoutPPE(Double.toString(Double.MAX_EXPONENT), Double.MAX_EXPONENT);

		final AbstractPrologTerm val = parser
				.nextSentence("298723987493287423423.00002342342300043324234324E+723864873268472323.");
		assertEquals(PrologTermType.ATOM, val.getType());
		assertEquals(PrologFloatNumber.class, val.getClass());
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
		final AbstractPrologTerm term = parser
				.nextSentence("date(Day,may,2001).");

		Mockito.verify(mock, Mockito.times(1)).processNewStructure(
				Mockito.any(PrologStructure.class));

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
		assertEquals(2001,
				((PrologIntegerNumber) struct.getElement(2)).getValue());
	}

	@Test
	public void testParseList() throws Exception {
		PrologList list = (PrologList) parser.nextSentence("[].");
		assertEquals(PrologTermType.LIST, list.getType());
		assertEquals(PrologList.class, list.getClass());
		assertTrue(((PrologList) list).isNullList());

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
		PrologStructure struct = (PrologStructure) parser
				.nextSentence("hello:-world.");

		Mockito.verify(mock, Mockito.times(1)).processNewStructure(
				Mockito.any(PrologStructure.class));

		assertEquals(PrologStructure.class, struct.getClass());
		assertEquals(PrologTermType.OPERATOR, struct.getFunctor().getType());
		assertEquals(":-", struct.getFunctor().getText());
		assertEquals(2, struct.getArity());
		assertEquals(OperatorType.XFX,
				((Operator) struct.getFunctor()).getOperatorType());
		assertEquals("hello", struct.getElement(0).getText());
		assertEquals("world", struct.getElement(1).getText());

		Mockito.reset(mock);
		struct = (PrologStructure) parser.nextSentence(":-test.");

		Mockito.verify(mock, Mockito.times(1)).processNewStructure(
				Mockito.any(PrologStructure.class));

		assertEquals(PrologStructure.class, struct.getClass());
		assertEquals(PrologTermType.OPERATOR, struct.getFunctor().getType());
		assertEquals(":-", struct.getFunctor().getText());
		assertEquals(1, struct.getArity());
		assertEquals(OperatorType.FX,
				((Operator) struct.getFunctor()).getOperatorType());
		assertEquals("test", struct.getElement(0).getText());

		Mockito.reset(mock);
		struct = (PrologStructure) parser.nextSentence("X is X+1.");

		Mockito.verify(mock, Mockito.times(2)).processNewStructure(
				Mockito.any(PrologStructure.class));

		assertEquals(PrologStructure.class, struct.getClass());
		assertEquals(PrologTermType.OPERATOR, struct.getFunctor().getType());
		assertEquals("is", struct.getFunctor().getText());
		assertEquals(2, struct.getArity());
		assertEquals(OperatorType.XFX,
				((Operator) struct.getFunctor()).getOperatorType());
		assertEquals("X", struct.getElement(0).getText());
		assertEquals(PrologTermType.STRUCT, struct.getElement(1).getType());
		assertEquals("+", ((PrologStructure) struct.getElement(1)).getFunctor()
				.getText());
		assertEquals(OperatorType.YFX,
				((Operator) ((PrologStructure) struct.getElement(1))
						.getFunctor()).getOperatorType());
		assertEquals(2, ((PrologStructure) struct.getElement(1)).getArity());
		assertEquals("X", ((PrologStructure) struct.getElement(1))
				.getElement(0).getText());
		assertEquals("1", ((PrologStructure) struct.getElement(1))
				.getElement(1).getText());
	}

	@Test
	public void testComments() throws Exception {
		AbstractPrologTerm term = parser
				.nextSentence("  %   zero line %%%% misc%%%% \n%first line\n%second line\n\nhello:-world.%test\n:-test.");
		AbstractPrologTerm term2 = parser.nextSentence();

		assertEquals("'hello' :- 'world'", term.toString());
		assertEquals(":- 'test'", term2.toString());

		assertNull(parser.nextSentence());

	}

	@Test
	public void testParseFileAsStream() throws Exception {
		final PrologCharDataSource reader = new PrologCharDataSource(getClass()
				.getResourceAsStream("sec811.pro"));
		try {
			AbstractPrologTerm term = parser.nextSentence(reader);
			assertNotNull(term);

			int index = 1;
			while (true) {
				try {
					term = parser.nextSentence();
					if (term == null)
						break;
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
	public void testOperatorHierarchy() throws Exception {
		final Map<String, OperatorContainer> operators = new HashMap<String, OperatorContainer>();

		final ParserContext contextStub = new ParserContext() {
			@Override
			public boolean hasOperatorStartsWith(
					final String operatorNameStartSubstring) {
				for (final String string : operators.keySet()) {
					if (string.startsWith(operatorNameStartSubstring))
						return true;
				}

				return false;
			}

			@Override
			public OperatorContainer findOperatorForName(
					final String operatorName) {
				return operators.get(operatorName);
			}

			@Override
			public boolean hasZeroArityPredicate(final String predicateName) {
				return false;
			}

			@Override
			public void processNewStructure(final PrologStructure structure) {
			}

		};

		final PrologParser parser = new PrologParser(contextStub);

		AbstractPrologTerm term = parser
				.nextSentence(":-op(800,xfx,'<===>').:-op(700,xfy,'v').:-op(600,xfy,'&').:-op(500,fy,'~').~(A&B)<===> ~A v ~B.");
		AbstractPrologTerm lastterm = null;
		while (term != null) {
			lastterm = term;
			if (term.getType() == PrologTermType.STRUCT) {
				final PrologStructure structure = (PrologStructure) term;
				if (structure.getArity() == 1
						&& structure.getFunctor().getText().equals(":-")) {
					final PrologStructure operatorstructure = (PrologStructure) structure
							.getElement(0);
					if (operatorstructure.getArity() == 3
							&& operatorstructure.getFunctor().getText()
									.equals("op")) {
						final Operator newoperator = new Operator(
								(int) ((PrologIntegerNumber) operatorstructure
										.getElement(0)).getValue(),
								OperatorType.getForName(operatorstructure
										.getElement(1).getText()),
								operatorstructure.getElement(2).getText());

						OperatorContainer container = operators.get(newoperator
								.getText());
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

		final PrologStructure leftBranch = (PrologStructure) ((PrologStructure) lastterm)
				.getElement(0);
		final PrologStructure rightBranch = (PrologStructure) ((PrologStructure) lastterm)
				.getElement(1);

		assertEquals(500, leftBranch.getPriority());
		assertEquals("~", leftBranch.getText());
		assertEquals(1, leftBranch.getArity());

		assertEquals(700, rightBranch.getPriority());
		assertEquals("v", rightBranch.getText());
		assertEquals(2, rightBranch.getArity());

		final PrologStructure leftBranch2 = (PrologStructure) leftBranch
				.getElement(0);
		final PrologStructure rightBranchL = (PrologStructure) rightBranch
				.getElement(0);
		final PrologStructure rightBranchR = (PrologStructure) rightBranch
				.getElement(1);

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
				Channels.newChannel(getClass()
						.getResourceAsStream("sec812.pro")));
		AbstractPrologTerm term = null;
		int index = 0;
		try {
			term = parser.nextSentence(reader);
			assertNotNull(term);
			index++;

			while (true) {
				try {
					term = parser.nextSentence();
					if (term == null)
						break;
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
				Channels.newChannel(getClass()
						.getResourceAsStream("sec816.pro")));
		try {
			term = parser.nextSentence(reader2);
			assertNotNull(term);

			index = 1;
			while (true) {
				try {
					term = parser.nextSentence();
					if (term == null)
						break;
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
}
