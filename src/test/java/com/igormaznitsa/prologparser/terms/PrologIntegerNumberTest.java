package com.igormaznitsa.prologparser.terms;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;

public class PrologIntegerNumberTest extends AbstractPrologParserTest {

	@Test
	public void testToString() {
		assertEquals("12932312312", new PrologIntegerNumber(12932312312L).toString());
		assertEquals("-123123",new PrologIntegerNumber(-123123L).toString());
		assertEquals("786873241", new PrologIntegerNumber("786873241").toString());
		assertEquals("-23432", new PrologIntegerNumber("-23432").toString());
	}

	@Test
	public void testNeg() {
		assertEquals(-66324377324L, ((PrologIntegerNumber)new PrologIntegerNumber(66324377324L).neg()).getValue());
		assertEquals(21334324324L, ((PrologIntegerNumber)new PrologIntegerNumber("-21334324324").neg()).getValue());
	}

	@Test
	public void testPrologIntegerNumberString() {
		try {
			new PrologIntegerNumber(null);
			fail("Must throw NPE if the text is null");
		}catch(NullPointerException ex){}
		
		try {
			new PrologIntegerNumber("wrong value");
			fail("Must throw IAE when wrong text");
		}catch(IllegalArgumentException ex){}

		try {
			new PrologIntegerNumber("");
			fail("Must throw IAE when empty text");
		}catch(IllegalArgumentException ex){}
		
		assertEquals(78621837612L, new PrologIntegerNumber("78621837612").getValue());
		assertEquals(-121231234214L, new PrologIntegerNumber("-121231234214").getValue());
		assertEquals(0L, new PrologIntegerNumber("-0").getValue());
		assertEquals(0L, new PrologIntegerNumber("-0000000").getValue());
		assertEquals(0L, new PrologIntegerNumber("00000").getValue());
	}

	@Test
	public void testPrologIntegerNumberLong() {
		assertEquals(2397498234L, new PrologIntegerNumber(2397498234L).getValue());
		assertEquals(-987987234L, new PrologIntegerNumber(-987987234L).getValue());
	}

	@Test
	public void testGetValue() {
		assertEquals(9998837672L, new PrologIntegerNumber(9998837672L).getValue());
		assertEquals(-88878233243L, new PrologIntegerNumber("-88878233243").getValue());
	}

	@Test
	public void testGetType() {
		assertEquals(PrologTermType.ATOM, new PrologIntegerNumber(9798273423L).getType());
		assertEquals(PrologTermType.ATOM, new PrologIntegerNumber("24324324").getType());
	}

	@Test
	public void testGetText() {
		assertEquals("9879823432", new PrologIntegerNumber("9879823432").getText());
		assertEquals("-2342343243", new PrologIntegerNumber(-2342343243L).getText());
	}

	@Test
	public void testGetPriority() {
		assertEquals(0, new PrologIntegerNumber(97239847324L).getPriority());
		assertEquals(0, new PrologIntegerNumber("-34324").getPriority());
	}

}
