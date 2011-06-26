package com.igormaznitsa.prologparser.terms;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;

public class PrologFloatNumberTest extends AbstractPrologParserTest {

	@Test
	public void testToString() {
		assertEquals("-0.0", new PrologFloatNumber(-0.0d).toString());
		assertEquals("-0.0", new PrologFloatNumber("-0.0").toString());
		assertEquals("-21.008", new PrologFloatNumber(-21.008d).toString());
		assertEquals("231.221", new PrologFloatNumber("231.221").toString());
	}

	@Test
	public void testNeg() {
		assertEquals(345.223d, ((PrologFloatNumber) new PrologFloatNumber(
				-345.223d).neg()).getValue(), 0d);
		assertEquals(-0.0003d, ((PrologFloatNumber) new PrologFloatNumber(
				0.0003d).neg()).getValue(), 0d);
	}

	@Test
	public void testGetValue() {
		assertEquals(-345.223d, new PrologFloatNumber("-345.223").getValue(),
				0d);
		assertEquals(0.0003d, new PrologFloatNumber(0.0003d).getValue(), 0d);
	}

	@Test
	public void testGetType() {
		assertEquals(PrologTermType.ATOM,
				new PrologFloatNumber(234.23d).getType());
		assertEquals(PrologTermType.ATOM,
				new PrologFloatNumber("234").getType());
	}

	@Test
	public void testGetText() {
		assertEquals("234.23", new PrologFloatNumber(234.23d).getText());
		assertEquals("-0.00223", new PrologFloatNumber(-0.00223).getText());
	}

	@Test
	public void testGetPriority() {
		assertEquals(0, new PrologFloatNumber(23412213.002131d).getPriority());
		assertEquals(0, new PrologFloatNumber(-000.002131d).getPriority());
	}

	@Test
	public void testPrologFloatNumber() {
		new PrologFloatNumber(0.1234221d);
		new PrologFloatNumber(-0.00021234221d);
		new PrologFloatNumber("0.12342211");
		new PrologFloatNumber("-2.00021234221");

		try {
			new PrologFloatNumber("wrong number");
			fail("Must throw NFE on a text nonnumeric value");
		} catch (NumberFormatException ex) {
		}

		try {
			new PrologFloatNumber(null);
			fail("Must throw NPE on null text data");
		} catch (NullPointerException ex) {
		}
	}

}
