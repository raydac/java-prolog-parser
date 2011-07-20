package com.igormaznitsa.prologparser.terms;

import static org.junit.Assert.*;

import java.math.BigInteger;

import org.junit.Test;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;

public class PrologIntegerNumberTest extends AbstractPrologParserTest {

	@Test
	public void testToString() {
		final PrologIntegerNumber test = new PrologIntegerNumber("8192739872198741213");
		assertNotNull(test.getText());
		assertEquals(test.getText(), test.toString());
	}

	@Test
	public void testNeg() {
		assertEquals(-66324377324L, ((PrologIntegerNumber)new PrologIntegerNumber("66324377324").neg()).getValue().longValue());
		assertEquals(21334324324L, ((PrologIntegerNumber)new PrologIntegerNumber("-21334324324").neg()).getValue().longValue());
	}

	@Test
	public void testPrologIntegerNumberLong() {
		assertEquals(9923L, new PrologIntegerNumber(9923L).getValue().longValue());
		assertEquals(-12343L, new PrologIntegerNumber(-12343L).getValue().longValue());
	}
	
	@Test
	public void testPrologIntegerNumberString() {
		try {
			new PrologIntegerNumber((String)null);
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
		
		assertEquals(78621837612L, new PrologIntegerNumber("78621837612").getValue().longValue());
		assertEquals(-121231234214L, new PrologIntegerNumber("-121231234214").getValue().longValue());
		assertEquals(0L, new PrologIntegerNumber("-0").getValue().longValue());
		assertEquals(0L, new PrologIntegerNumber("-0000000").getValue().longValue());
		assertEquals(0L, new PrologIntegerNumber("00000").getValue().longValue());
	}

	@Test
	public void testPrologIntegerNumberBigInteger() {
		try {
			new PrologIntegerNumber((BigInteger)null);
			fail("Must throw NPE if the value is null");
		}catch(NullPointerException ex){}
		
		final BigInteger testValue = new BigInteger("829374982374812093209380194832984");
		
		assertEquals(testValue, new PrologIntegerNumber(testValue).getValue());
	}
	
	@Test
	public void testGetValue() {
		assertEquals(9998837672L, new PrologIntegerNumber(9998837672L).getValue().longValue());
		assertEquals(-88878233243L, new PrologIntegerNumber("-88878233243").getValue().longValue());
	}

	@Test
	public void testGetType() {
		assertEquals(PrologTermType.ATOM, new PrologIntegerNumber(new BigInteger("111")).getType());
		assertEquals(PrologTermType.ATOM, new PrologIntegerNumber("24324324").getType());
	}

	@Test
	public void testGetText() {
		assertEquals("9879823432", new PrologIntegerNumber(9879823432L).getText());
		assertEquals("-2342343243", new PrologIntegerNumber(-2342343243L).getText());
		assertEquals("-23", new PrologIntegerNumber("-0000023").getText());
		assertEquals("123", new PrologIntegerNumber("0000000000000000000000000123").getText());
	}

	@Test
	public void testGetPriority() {
		assertEquals(0, new PrologIntegerNumber("97239847324").getPriority());
		assertEquals(0, new PrologIntegerNumber(123L).getPriority());
	}

}
