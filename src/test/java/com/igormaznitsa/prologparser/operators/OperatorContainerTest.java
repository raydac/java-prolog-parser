package com.igormaznitsa.prologparser.operators;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;
import com.igormaznitsa.prologparser.terms.PrologTermType;

public class OperatorContainerTest extends AbstractPrologParserTest {

	@Test
	public void testGetPriority() {
		assertEquals(new OperatorContainer(new Operator(1000, OperatorType.FX,
				"<>")).getPriority(), 0);
	}

	@Test
	public void testToString() {
		final Operator operator = new Operator(100, OperatorType.FX, "<>");
		final OperatorContainer container = new OperatorContainer(operator);
		container.addOperator(new Operator(300, OperatorType.XFX, "<>"));
		container.addOperator(new Operator(800, OperatorType.YF, "<>"));
		assertEquals(
				"OperatorContainer [op(100,fx,'<>'). op(800,yf,'<>'). op(300,xfx,'<>').]",
				container.toString());
	}

	@Test
	public void testGetType() {
		assertEquals(PrologTermType.OPERATORS, new OperatorContainer(
				new Operator(1000, OperatorType.FX, "<>")).getType());
	}

	@Test
	public void testOperatorContainer() {
		try {
			new OperatorContainer(null);
			fail("Must throw NPE for null argument");
		} catch (NullPointerException ex) {
		}

		final Operator operator = new Operator(100, OperatorType.FX, "<>");
		final OperatorContainer container = new OperatorContainer(operator);
		assertEquals(1, container.size());
		assertSame(operator, container.getOperatorIfSingle());
	}

	@Test
	public void testAddOperator() {
		final Operator operatorFX = new Operator(100, OperatorType.FX, "<>");
		final Operator operatorXFX = new Operator(400, OperatorType.XFX, "<>");
		final Operator operatorYF = new Operator(300, OperatorType.YF, "<>");
		final Operator otheroperatorYF = new Operator(300, OperatorType.YF,
				"><");

		final OperatorContainer container = new OperatorContainer(operatorFX);

		try {
			container.addOperator(null);
			fail("Must throw NPE for null argument");
		} catch (NullPointerException ex) {
		}

		try {
			container.addOperator(otheroperatorYF);
			fail("Must throw IAE for wrong operator name");
		} catch (IllegalArgumentException ex) {
		}

		assertEquals(1, container.size());
		assertTrue(container.addOperator(operatorXFX));
		assertEquals(2, container.size());
		assertTrue(container.addOperator(operatorYF));
		assertEquals(3, container.size());

		assertFalse(container.addOperator(operatorXFX));

		assertSame(operatorFX, container.getOperatorForType(OperatorType.FX));
		assertSame(operatorXFX, container.getOperatorForType(OperatorType.XFX));
		assertSame(operatorYF, container.getOperatorForType(OperatorType.YF));
	}

	@Test
	public void testRemoveAll() {
		final Operator operatorFX = new Operator(100, OperatorType.FX, "<>");
		final Operator operatorXFX = new Operator(400, OperatorType.XFX, "<>");
		final Operator operatorYF = new Operator(300, OperatorType.YF, "<>");

		final OperatorContainer container = new OperatorContainer(operatorFX);

		try {
			container.addOperator(null);
			fail("Must throw NPE for null argument");
		} catch (NullPointerException ex) {
		}

		assertTrue(container.addOperator(operatorXFX));
		assertTrue(container.addOperator(operatorYF));

		assertEquals(3, container.size());
		container.removeAll();
		assertEquals(0, container.size());
		assertNull(container.getOperatorForType(OperatorType.FX));
		assertNull(container.getOperatorForType(OperatorType.XFX));
		assertNull(container.getOperatorForType(OperatorType.YF));
	}

	@Test
	public void testRemove() {
		final Operator operatorFX = new Operator(100, OperatorType.FX, "<>");
		final Operator operatorXFX = new Operator(400, OperatorType.XFX, "<>");
		final Operator operatorYF = new Operator(300, OperatorType.YF, "<>");
		final Operator otheroperatorXFX = new Operator(400, OperatorType.XFX,
				"><");

		final OperatorContainer container = new OperatorContainer(operatorFX);
		container.addOperator(operatorXFX);
		container.addOperator(operatorYF);

		assertEquals(3, container.size());

		assertTrue(container.remove(operatorFX));
		assertEquals(2, container.size());
		assertNull(container.getOperatorForSimilarType(OperatorType.FX));

		try {
			container.remove(otheroperatorXFX);
			fail("Must throw IAE for operator wrong name");
		} catch (IllegalArgumentException ex) {
		}

		assertTrue(container.remove(operatorXFX));
		assertEquals(1, container.size());
		assertNull(container.getOperatorForSimilarType(OperatorType.XFX));

		assertTrue(container.remove(operatorYF));
		assertEquals(0, container.size());
		assertNull(container.getOperatorForSimilarType(OperatorType.YF));
	}

	@Test
	public void testSize() {
		final Operator operatorFX = new Operator(100, OperatorType.FX, "<>");
		final Operator operatorXFX = new Operator(400, OperatorType.XFX, "<>");
		final Operator operatorYF = new Operator(300, OperatorType.YF, "<>");

		final OperatorContainer container = new OperatorContainer(operatorFX);

		assertEquals(1, container.size());
		container.removeAll();
		assertEquals(0, container.size());
		container.addOperator(operatorFX);
		assertEquals(1, container.size());
		container.addOperator(operatorXFX);
		assertEquals(2, container.size());
		container.addOperator(operatorYF);
		assertEquals(3, container.size());
	}

	@Test
	public void testGetOperatorIfSingle() {
		final Operator operatorFX = new Operator(100, OperatorType.FX, "<>");
		final Operator operatorXFX = new Operator(400, OperatorType.XFX, "<>");
		final Operator operatorYF = new Operator(300, OperatorType.YF, "<>");

		final OperatorContainer container = new OperatorContainer(operatorFX);

		assertSame(operatorFX, container.getOperatorIfSingle());
		container.addOperator(operatorYF);
		assertNull(container.getOperatorIfSingle());
		container.addOperator(operatorXFX);
		assertNull(container.getOperatorIfSingle());
		container.removeAll();
		assertNull(container.getOperatorIfSingle());
	}

	@Test
	public void testFindCompatibleOperator() {
		final Operator operatorFX = new Operator(100, OperatorType.FX, "<>");
		final Operator operatorXFX = new Operator(400, OperatorType.XFX, "<>");
		final Operator operatorYF = new Operator(300, OperatorType.YF, "<>");

		final OperatorContainer container = new OperatorContainer(operatorFX);

		container.addOperator(operatorXFX);
		container.addOperator(operatorYF);

		assertSame(operatorFX, container.findCompatibleOperator(false, true));
		assertSame(operatorXFX, container.findCompatibleOperator(true, true));
		assertSame(operatorYF, container.findCompatibleOperator(true, false));
		assertNull(container.findCompatibleOperator(false, false));

		assertTrue(container.remove(operatorXFX));

		assertSame(operatorFX, container.findCompatibleOperator(false, true));
		assertSame(operatorFX, container.findCompatibleOperator(true, true));
		assertSame(operatorYF, container.findCompatibleOperator(true, false));
		assertNull(container.findCompatibleOperator(false, false));

		assertTrue(container.remove(operatorFX));

		assertSame(operatorYF, container.findCompatibleOperator(false, true));
		assertSame(operatorYF, container.findCompatibleOperator(true, true));
		assertSame(operatorYF, container.findCompatibleOperator(true, false));
		assertNull(container.findCompatibleOperator(false, false));

		assertTrue(container.remove(operatorYF));

		assertNull(container.findCompatibleOperator(false, true));
		assertNull(container.findCompatibleOperator(true, true));
		assertNull(container.findCompatibleOperator(true, false));
		assertNull(container.findCompatibleOperator(false, false));
	}

	@Test
	public void testGetOperatorForType() {
		final Operator operatorFX = new Operator(100, OperatorType.FX, "<>");
		final Operator operatorXFX = new Operator(400, OperatorType.XFX, "<>");
		final Operator operatorYF = new Operator(300, OperatorType.YF, "<>");

		final OperatorContainer container = new OperatorContainer(operatorFX);

		assertNull(container.getOperatorForType(OperatorType.XFX));
		assertNull(container.getOperatorForType(OperatorType.YF));

		container.addOperator(operatorXFX);
		container.addOperator(operatorYF);

		assertSame(operatorFX, container.getOperatorForType(OperatorType.FX));
		assertSame(operatorXFX, container.getOperatorForType(OperatorType.XFX));
		assertSame(operatorYF, container.getOperatorForType(OperatorType.YF));
		assertNull(container.getOperatorForType(OperatorType.YFX));
		assertNull(container.getOperatorForType(OperatorType.XF));
	}

	@Test
	public void testGetOperatorForSimilarType() {
		final Operator operatorFX = new Operator(100, OperatorType.FX, "<>");
		final Operator operatorXFX = new Operator(400, OperatorType.XFX, "<>");
		final Operator operatorYF = new Operator(300, OperatorType.YF, "<>");

		final OperatorContainer container = new OperatorContainer(operatorFX);
		container.addOperator(operatorXFX);
		container.addOperator(operatorYF);

		try {
			container.getOperatorForSimilarType(null);
			fail("Must throw NPE for null argument");
		} catch (NullPointerException ex) {
		}

		assertSame(operatorFX,
				container.getOperatorForSimilarType(OperatorType.FY));
		assertSame(operatorXFX,
				container.getOperatorForSimilarType(OperatorType.YFX));
		assertTrue(container.removeOperatorForType(OperatorType.FX));
		assertNull(container.getOperatorForSimilarType(OperatorType.FY));

	}

	@Test
	public void testRemoveOperatorForType() {
		final Operator operatorFX = new Operator(100, OperatorType.FX, "<>");
		final Operator operatorXFX = new Operator(400, OperatorType.XFX, "<>");
		final Operator operatorYF = new Operator(300, OperatorType.YF, "<>");

		final OperatorContainer container = new OperatorContainer(operatorFX);
		container.addOperator(operatorXFX);
		container.addOperator(operatorYF);

		try {
			container.remove(null);
			fail("Must throw NPE for null argument");
		} catch (NullPointerException ex) {
		}

		assertTrue(container.removeOperatorForType(OperatorType.FX));
		assertNull(container.getOperatorForType(OperatorType.FX));
		assertTrue(container.removeOperatorForType(OperatorType.XFX));
		assertNull(container.getOperatorForType(OperatorType.XFX));
		assertTrue(container.removeOperatorForType(OperatorType.YF));
		assertNull(container.getOperatorForType(OperatorType.YF));
		assertFalse(container.removeOperatorForType(OperatorType.YFX));
	}

	@Test
	public void testGetText() {
		assertEquals("<>", new OperatorContainer(new Operator(1000,
				OperatorType.FX, "<>")).getText());
	}

}
