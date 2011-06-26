package com.igormaznitsa.prologparser.terms;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;

public class PrologAtomTest extends AbstractPrologParserTest {

	@Test
	public void testGetPriority() {
		assertEquals(new PrologAtom("Hello").getPriority(), 0);
	}

	@Test
	public void testToString() {
		assertEquals("\'Hello World\'", new PrologAtom("Hello World").toString());
		assertEquals("\'Hello\\nWorld\'", new PrologAtom("Hello\nWorld").toString());
		assertEquals("\'Hello\\\\nWorld\'", new PrologAtom("Hello\\\nWorld").toString());
		assertEquals("\'Hello\\tWorld\'", new PrologAtom("Hello\tWorld").toString());
		assertEquals("\'!\'", new PrologAtom("!").toString());
	}

	@Test
	public void testGetType() {
		assertEquals(PrologTermType.ATOM, new PrologAtom("Hello Prolog").getType());
	}

	@Test
	public void testPrologAtom() {
		new PrologAtom("Hello");
		try {
			new PrologAtom(null);
			fail("Null name must throw NPE");
		} catch (NullPointerException ex) {
		}
	}

}
