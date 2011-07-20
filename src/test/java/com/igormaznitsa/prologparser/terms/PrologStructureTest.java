package com.igormaznitsa.prologparser.terms;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

import java.util.Map;

import org.junit.Test;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.operators.OperatorType;

public class PrologStructureTest extends AbstractPrologParserTest {

	@Test
	public void testGetPriority() {
		assertEquals(new PrologStructure("hello").getPriority(), 0);
		assertEquals(0,
				new PrologStructure(new PrologAtom("hello")).getPriority());

		final Operator testOperator = new Operator(666, OperatorType.FX, ":::");

		assertEquals(666,
				new PrologStructure(testOperator,
						new AbstractPrologTerm[] { new PrologAtom("test") })
						.getPriority());
	}

	@Test
	public void testToString() {
		assertEquals("\'Hello World\'()", new PrologStructure(new PrologAtom(
				"Hello World")).toString());
		assertEquals("!",new PrologStructure(new PrologAtom("!")).toString());

		assertEquals("'test'('', '', '')", new PrologStructure(new PrologAtom(
				"test"), 3).toString());
		assertEquals("'123'('first', 123.0, [], _)", new PrologStructure(
				new PrologAtom("123"), new AbstractPrologTerm[] {
						new PrologAtom("first"), new PrologFloatNumber(123d),
						new PrologList(), new PrologVariable() }).toString());

		final Map<String, OperatorContainer> systemOperators = PrologParser
				.getSystemOperators();
		assertEquals("'hello' :- 'world'", new PrologStructure(systemOperators
				.get(":-").getOperatorForType(OperatorType.XFX),
				new AbstractPrologTerm[] { new PrologAtom("hello"),
						new PrologAtom("world") }).toString());
		assertEquals(":- 'hello'",
				new PrologStructure(systemOperators.get(":-")
						.getOperatorForType(OperatorType.FX),
						new AbstractPrologTerm[] { new PrologAtom("hello") })
						.toString());
		assertEquals(
				"(- 10) * (1 + 2)",
				new PrologStructure(
						systemOperators.get("*").getOperatorForType(
								OperatorType.YFX),
						new AbstractPrologTerm[] {
								new PrologStructure(
										systemOperators.get("-")
												.getOperatorForType(
														OperatorType.FX),
										new AbstractPrologTerm[] { new PrologIntegerNumber("10") }),
								new PrologStructure(systemOperators.get("+")
										.getOperatorForType(OperatorType.YFX),
										new AbstractPrologTerm[] {
												new PrologIntegerNumber("1"),
												new PrologIntegerNumber("2") }) })
						.toString());

		assertEquals(
				"- (- 10)",
				new PrologStructure(
						systemOperators.get("-").getOperatorForType(
								OperatorType.FX),
						new AbstractPrologTerm[] { new PrologStructure(
								systemOperators.get("-").getOperatorForType(
										OperatorType.FX),
								new AbstractPrologTerm[] { new PrologIntegerNumber(
										"10") }) }).toString());

		assertEquals(
				"\\ (\\+ 10)",
				new PrologStructure(
						systemOperators.get("\\").getOperatorForType(
								OperatorType.FY),
						new AbstractPrologTerm[] { new PrologStructure(
								systemOperators.get("\\+").getOperatorForType(
										OperatorType.FY),
								new AbstractPrologTerm[] { new PrologIntegerNumber(
										"10") }) }).toString());
		assertEquals(
				"(10 .) .",
				new PrologStructure(
						systemOperators.get(".").getOperatorForType(
								OperatorType.XF),
						new AbstractPrologTerm[] { new PrologStructure(
								systemOperators.get(".").getOperatorForType(
										OperatorType.XF),
								new AbstractPrologTerm[] { new PrologIntegerNumber(
										"10") }) }).toString());

		final Operator operatorYF = new Operator(800, OperatorType.YF, "!");
		final Operator operatorYF2 = new Operator(1000, OperatorType.YF, "!!");

		assertEquals(
				"(10 !!) !",
				new PrologStructure(
						operatorYF,
						new AbstractPrologTerm[] { new PrologStructure(
								operatorYF2,
								new AbstractPrologTerm[] { new PrologIntegerNumber(
										"10") }) }).toString());

		final Operator operatorXFX = new Operator(800, OperatorType.XFX, "$");
		final Operator operatorXFX2 = new Operator(1000, OperatorType.XFX, "$$");

		assertEquals("(10 $$ 20) $ (5 $ 30)",
				new PrologStructure(operatorXFX, new AbstractPrologTerm[] {
						new PrologStructure(operatorXFX2,
								new AbstractPrologTerm[] {
										new PrologIntegerNumber("10"),
										new PrologIntegerNumber("20") }),
						new PrologStructure(operatorXFX,
								new AbstractPrologTerm[] {
										new PrologIntegerNumber("5"),
										new PrologIntegerNumber("30") }) })
						.toString());

		final Operator operatorXFY = new Operator(800, OperatorType.XFY, "$");
		final Operator operatorXFY2 = new Operator(1000, OperatorType.XFY, "$$");

		assertEquals("10 $ 20 $$ 5 $ 30",
				new PrologStructure(operatorXFY2, new AbstractPrologTerm[] {
						new PrologStructure(operatorXFY,
								new AbstractPrologTerm[] {
										new PrologIntegerNumber("10"),
										new PrologIntegerNumber("20") }),
						new PrologStructure(operatorXFY,
								new AbstractPrologTerm[] {
										new PrologIntegerNumber("5"),
										new PrologIntegerNumber("30") }) })
						.toString());
	}

	@Test
	public void testGetType() {
		assertEquals(PrologTermType.STRUCT,
				new PrologStructure("hello").getType());
		assertEquals(PrologTermType.STRUCT, new PrologStructure(new PrologAtom(
				"hello")).getType());
		final Operator testOperator = new Operator(666, OperatorType.FX, ":::");
		assertEquals(PrologTermType.STRUCT, new PrologStructure(testOperator,
				new AbstractPrologTerm[] { new PrologAtom("test") }).getType());
	}

	@Test
	public void testPrologStructureAbstractPrologTermAbstractPrologTermArray() {
		final AbstractPrologTerm[] testterms = new AbstractPrologTerm[] {
				new PrologAtom("test1"), new PrologAtom("test2"),
				new PrologAtom("test3") };
		final AbstractPrologTerm[] testtermswithnull = new AbstractPrologTerm[] {
				new PrologAtom("test1"), null, new PrologAtom("test3") };

		try {
			new PrologStructure(null, testterms);
			fail("Must throw NPE for null functor");
		} catch (NullPointerException ex) {
		}

		try {
			new PrologStructure(new PrologAtom("hello"), null);
			fail("Must throw NPE for null array");
		} catch (NullPointerException ex) {
		}

		try {
			new PrologStructure(new PrologAtom("hello"), testtermswithnull);
			fail("Must throw NPE for array contains null");
		} catch (NullPointerException ex) {
		}

		try {
			new PrologStructure(new PrologFloatNumber(0.0d), testtermswithnull);
			fail("Must throw IAE for numeric functor");
		} catch (IllegalArgumentException ex) {
		}

		try {
			new PrologStructure(new PrologList(), testtermswithnull);
			fail("Must throw IAE for list functor");
		} catch (IllegalArgumentException ex) {
		}

		final PrologAtom functoratom = new PrologAtom("functor");
		PrologStructure struct = new PrologStructure(functoratom, testterms);
		assertSame(functoratom, struct.getFunctor());
		assertEquals(testterms.length, struct.getArity());

		final Operator functoroperator = new Operator(222, OperatorType.XFX,
				">>>");
		struct = new PrologStructure(functoroperator, testterms);
		assertSame(functoroperator, struct.getFunctor());
		assertEquals(testterms.length, struct.getArity());

		final AbstractPrologTerm etalon = testterms[1];
		struct.setElement(1, new PrologAtom("new"));
		assertSame(etalon, testterms[1]);
	}

	@Test
	public void testPrologStructureString() {
		try {
			new PrologStructure((String) null);
			fail("Musth throw NPE for null argument");
		} catch (NullPointerException ex) {
		}

		final PrologStructure struct = new PrologStructure("Hello World");
		assertNotNull(struct.getFunctor());
		assertEquals(PrologTermType.ATOM, struct.getFunctor().getType());
		assertEquals("Hello World", struct.getFunctor().getText());

		final PrologStructure struct2 = new PrologStructure("1111");
		assertNotNull(struct2.getFunctor());
		assertEquals(PrologTermType.ATOM, struct2.getFunctor().getType());
		assertFalse(struct2.getFunctor() instanceof AbstractPrologNumericTerm);
		assertEquals("1111", struct2.getFunctor().getText());

	}

	@Test
	public void testPrologStructureAbstractPrologTerm() {
		final PrologAtom atom = new PrologAtom("atom1");
		final Operator operator = new Operator(6, OperatorType.FX, "...");

		try {
			new PrologStructure((AbstractPrologTerm) null);
			fail("Must throw NPE for null argument");
		} catch (NullPointerException ex) {
		}

		try {
			new PrologStructure(new PrologFloatNumber(0.0d));
			fail("Must throw NPE for numeric argument");
		} catch (IllegalArgumentException ex) {
		}

		try {
			new PrologStructure(new PrologList());
			fail("Must throw NPE for list argument");
		} catch (IllegalArgumentException ex) {
		}

		assertSame(atom, new PrologStructure(atom).getFunctor());
		assertSame(operator, new PrologStructure(operator).getFunctor());
	}

	@Test
	public void testPrologStructureAbstractPrologTermInt() {
		try {
			new PrologStructure(null, 4);
			fail("Must throw NPE for the null functor");
		} catch (NullPointerException ex) {
		}

		try {
			new PrologStructure(new PrologAtom("test"), -1);
			fail("Must throw IAE for the negative arity");
		} catch (IllegalArgumentException ex) {
		}

		try {
			new PrologStructure(new PrologVariable(), 10);
			fail("Must throw IAE for variable as functor");
		} catch (IllegalArgumentException ex) {
		}

		try {
			new PrologStructure(new PrologList(), 10);
			fail("Must throw IAE for variable as list");
		} catch (IllegalArgumentException ex) {
		}

		try {
			new PrologStructure(new PrologIntegerNumber("5"), 10);
			fail("Must throw IAE for variable as integer");
		} catch (IllegalArgumentException ex) {
		}

		try {
			new PrologStructure(new PrologFloatNumber(5.0d), 10);
			fail("Must throw IAE for variable as float");
		} catch (IllegalArgumentException ex) {
		}

		final PrologAtom testAtom = new PrologAtom("test");
		assertEquals(0, new PrologStructure(testAtom, 0).getArity());

		final PrologStructure struct = new PrologStructure(testAtom, 10);
		for (int index = 0; index < 10; index++) {
			assertEquals("", struct.getElement(index).getText());
		}
	}

	@Test
	public void testGetArity() {
		final AbstractPrologTerm[] testterms = new AbstractPrologTerm[] {
				new PrologAtom("test1"), new PrologAtom("test2"),
				new PrologAtom("test3") };

		assertEquals(testterms.length, new PrologStructure(new PrologAtom(
				"hello"), testterms).getArity());
		assertEquals(8,
				new PrologStructure(new PrologAtom("test"), 8).getArity());
	}

	@Test
	public void testGetElement() {
		final String[] strings = new String[] { "test1", "test2", "test3",
				"test4", "test5", "test6", "test7", "test8" };
		final AbstractPrologTerm[] terms = new AbstractPrologTerm[strings.length];
		for (int i = 0; i < strings.length; i++) {
			terms[i] = new PrologAtom(strings[i]);
		}

		final PrologStructure structure = new PrologStructure(new PrologAtom(
				"functor"), terms);
		for (int li = 0; li < strings.length; li++) {
			assertEquals(strings[li], structure.getElement(li).getText());
		}
		try {
			structure.getElement(-1);
			fail("Must throw ArrayIndexOutOfBoundsException for negative index");
		} catch (ArrayIndexOutOfBoundsException ex) {
		}

		try {
			structure.getElement(strings.length);
			fail("Must throw ArrayIndexOutOfBoundsException for the index equals arity");
		} catch (ArrayIndexOutOfBoundsException ex) {
		}
	}

	@Test
	public void testSetElement() {
		final String[] strings = new String[] { "test1", "test2", "test3",
				"test4", "test5", "test6", "test7", "test8" };
		final AbstractPrologTerm[] terms = new AbstractPrologTerm[strings.length];
		for (int i = 0; i < strings.length; i++) {
			terms[i] = new PrologAtom(strings[i]);
		}

		final PrologStructure structure = new PrologStructure(new PrologAtom(
				"functor"), terms.length);
		for (int li = 0; li < terms.length; li++)
			structure.setElement(li, terms[li]);

		try {
			structure.setElement(-1, new PrologList());
			fail("Must throw ArrayIndexOutOfBoundsException for negative index");
		} catch (ArrayIndexOutOfBoundsException ex) {
		}

		try {
			structure.setElement(terms.length, new PrologList());
			fail("Must throw ArrayIndexOutOfBoundsException for the index equals arity");
		} catch (ArrayIndexOutOfBoundsException ex) {
		}

		for (int li = 0; li < terms.length; li++) {
			assertSame(terms[li], structure.getElement(li));
		}
	}

	@Test
	public void testGetFunctor() {
		final PrologAtom functoratom = new PrologAtom("testfunctor");
		final Operator functoroperator = new Operator(666, OperatorType.FX,
				":::");

		assertEquals(functoratom, new PrologStructure(functoratom).getFunctor());
		assertEquals(functoratom,
				new PrologStructure(functoratom, 3).getFunctor());
		assertEquals(new PrologStructure(functoroperator).getFunctor(),
				functoroperator);
	}

	@Test
	public void testGetText() {
		assertEquals("test", new PrologStructure("test").getText());
		assertEquals("test",
				new PrologStructure(new PrologAtom("test"), 5).getText());
		assertEquals("<<<", new PrologStructure(new Operator(222,
				OperatorType.FY, "<<<"), 5).getText());
	}

}
