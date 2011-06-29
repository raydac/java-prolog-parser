package com.igormaznitsa.prologparser.terms;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;

public class AbstractPrologTermTest extends AbstractPrologParserTest {

	private final static class StubAbstractPrologTermTest extends AbstractPrologTerm {

		public StubAbstractPrologTermTest(final String text) {
			super(text);
		}
		
		@Override
		public PrologTermType getType() {
			throw new UnsupportedOperationException("It's a stub");
		}
		
	}
	
	@Test
	public void testAbstractPrologTerm() {
		try {
			new StubAbstractPrologTermTest(null);
			fail("Must throw NPE for null text");
		}catch(NullPointerException ex){}
		
		assertEquals("test",new StubAbstractPrologTermTest("test").text);
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
