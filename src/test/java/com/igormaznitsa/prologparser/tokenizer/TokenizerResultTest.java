package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.tokenizer.TokenizerResult;
import com.igormaznitsa.prologparser.tokenizer.TokenizerState;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;


public class TokenizerResultTest {

  @Test
  public void testTokenizerResult() {
    assertThrows(NullPointerException.class, () -> new TokenizerResult(null, TokenizerState.ATOM, 1, 2));
    assertThrows(NullPointerException.class, () -> new TokenizerResult(new PrologAtom("test"), null, 1, 2));

    final PrologAtom testAtom = new PrologAtom("test");
    final TokenizerResult result = new TokenizerResult(testAtom,
        TokenizerState.ATOM, 1, 2);
    assertSame(testAtom, result.getResult());
    assertEquals(TokenizerState.ATOM, result.getTokenizerState());

  }

  @Test
  public void testGetTokenizerState() {
    final PrologAtom testAtom = new PrologAtom("test");
    final TokenizerResult result = new TokenizerResult(testAtom,
        TokenizerState.STRING, 1, 2);
    assertSame(testAtom, result.getResult());
    assertEquals(TokenizerState.STRING, result.getTokenizerState());
  }

  @Test
  public void testGetResult() {
    final PrologIntegerNumber testAtom = new PrologIntegerNumber("322323423");
    final TokenizerResult result = new TokenizerResult(testAtom,
        TokenizerState.LOOKFOR, 1, 2);
    assertSame(testAtom, result.getResult());
    assertEquals(TokenizerState.LOOKFOR, result.getTokenizerState());
  }

  @Test
  public void testGetTermType() {
    final PrologIntegerNumber testAtom = new PrologIntegerNumber("322323423");
    final TokenizerResult result = new TokenizerResult(testAtom,
        TokenizerState.LOOKFOR, 1, 2);
    assertSame(testAtom, result.getResult());
    assertEquals(PrologTermType.ATOM, result.getTermType());
  }

  @Test
  public void testGetStringPosition() {
    final TokenizerResult result = new TokenizerResult(new PrologAtom("test"), TokenizerState.LOOKFOR, 1, 2);
    assertEquals(1, result.getStringPosition());
  }

  @Test
  public void testGetLineNumber() {
    final TokenizerResult result = new TokenizerResult(new PrologAtom("test"), TokenizerState.LOOKFOR, 1, 2);
    assertEquals(2, result.getLineNumber());
  }
}
