package com.igormaznitsa.prologparser.terms;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;

public class AbstractPrologTermTest extends AbstractPrologParserTest {

	private final static class StubAbstractPrologTermTest extends AbstractPrologTerm {

		public StubAbstractPrologTermTest(final String text) {
			super(text);
		}
		
		public StubAbstractPrologTermTest(final String text, final int strPos, final int lineNum) {
			super(text,strPos, lineNum);
		}
		
		@Override
		public PrologTermType getType() {
			throw new UnsupportedOperationException("It's a stub");
		}
		
	}
	
	@Test
	public void testAbstractPrologTermString() {
		try {
			new StubAbstractPrologTermTest(null);
			fail("Must throw NPE for null text");
		}catch(NullPointerException ex){}

		final AbstractPrologTerm test = new StubAbstractPrologTermTest("test");
		assertEquals(-1, test.getStrPosition());
		assertEquals(-1, test.getLineNumber());
	}

	@Test
	public void testAbstractPrologTermStringIntInt() {
		try {
			new StubAbstractPrologTermTest(null, 1, 2);
			fail("Must throw NPE for null text");
		} catch(NullPointerException ex) {
		}
		
		final AbstractPrologTerm test = new StubAbstractPrologTermTest("test", 1, 2);
		assertEquals(1, test.getStrPosition());
		assertEquals(2, test.getLineNumber());
	}
	
	@Test
	public void testSetStrPosition() {
		final AbstractPrologTerm test = new StubAbstractPrologTermTest("test");
		test.setStrPosition(101);
		assertEquals(101,test.getStrPosition());
		test.setStrPosition(0);
		assertEquals(-1,test.getStrPosition());
		test.setStrPosition(-10);
		assertEquals(-1,test.getStrPosition());
	}

	@Test
	public void testGetStrPosition() {
		final AbstractPrologTerm test = new StubAbstractPrologTermTest("test",101,202);
		assertEquals(101,test.getStrPosition());
	}
	
	@Test
	public void testGetLineNumber() {
		final AbstractPrologTerm test = new StubAbstractPrologTermTest("test",101,202);
		assertEquals(202,test.getLineNumber());
	}
	
	@Test
	public void testSetLineNumber() {
		final AbstractPrologTerm test = new StubAbstractPrologTermTest("test");
		test.setLineNumber(101);
		assertEquals(101,test.getLineNumber());
		test.setLineNumber(0);
		assertEquals(-1,test.getLineNumber());
		test.setStrPosition(-10);
		assertEquals(-1,test.getLineNumber());
	}
	
	@Test
	public void testGetText() {
		assertEquals("Test text",new StubAbstractPrologTermTest("Test text").getText());
	}

	@Test
	public void testGetPriority() {
		assertEquals(0,new StubAbstractPrologTermTest("test").getPriority());
	}

	@Test
	public void testToString() {
		assertEquals("test \n hello", new StubAbstractPrologTermTest("test \n hello").toString());
	}

	@Test
	public void testSetLinkedObject() {
		final StubAbstractPrologTermTest stub = new StubAbstractPrologTermTest("test");
		assertNull(stub.linkedObject);
		stub.setLinkedObject("test_linked");
		assertEquals("test_linked", stub.linkedObject);
		stub.setLinkedObject(null);
		assertNull(stub.linkedObject);
	}

	@Test
	public void testGetLinkedObject() {
		final StubAbstractPrologTermTest stub = new StubAbstractPrologTermTest("test");
		assertNull(stub.getLinkedObject());
		stub.linkedObject = "testObject";
		assertEquals("testObject",stub.getLinkedObject());
	}

}
