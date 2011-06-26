package com.igormaznitsa.prologparser;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologTermType;

public class TokenizerResultTest {

	@Test
	public void testTokenizerResult() {
		try {
			new TokenizerResult(null, TokenizerState.ATOM);
			fail("Must throw NPE for null term");
		} catch (NullPointerException ex) {
		}

		try {
			new TokenizerResult(new PrologAtom("test"), null);
			fail("Must throw NPE for null state");
		} catch (NullPointerException ex) {
		}

		final PrologAtom testAtom = new PrologAtom("test");
		final TokenizerResult result = new TokenizerResult(testAtom,
				TokenizerState.ATOM);
		assertSame(testAtom, result.getResult());
		assertEquals(TokenizerState.ATOM, result.getTokenizerState());

	}

	@Test
	public void testGetTokenizerState() {
		final PrologAtom testAtom = new PrologAtom("test");
		final TokenizerResult result = new TokenizerResult(testAtom,
				TokenizerState.STRING);
		assertSame(testAtom, result.getResult());
		assertEquals(TokenizerState.STRING, result.getTokenizerState());
	}

	@Test
	public void testGetResult() {
		final PrologIntegerNumber testAtom = new PrologIntegerNumber(322323423);
		final TokenizerResult result = new TokenizerResult(testAtom,
				TokenizerState.LOOKFOR);
		assertSame(testAtom, result.getResult());
		assertEquals(TokenizerState.LOOKFOR, result.getTokenizerState());
	}

	@Test
	public void testGetTermType() {
		final PrologIntegerNumber testAtom = new PrologIntegerNumber(322323423);
		final TokenizerResult result = new TokenizerResult(testAtom,
				TokenizerState.LOOKFOR);
		assertSame(testAtom, result.getResult());
		assertEquals(PrologTermType.ATOM, result.getTermType());
	}

}
