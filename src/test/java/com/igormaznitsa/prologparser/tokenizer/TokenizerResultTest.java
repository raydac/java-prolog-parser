package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologInt;
import com.igormaznitsa.prologparser.terms.TermType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@SuppressWarnings("unchecked")
public class TokenizerResultTest {

  @Test
  public void testTokenizerResult() {
    assertThrows(NullPointerException.class, () -> new TokenizerResult(null, TokenizerState.ATOM, 2, 1));
    assertThrows(NullPointerException.class, () -> new TokenizerResult(new PrologAtom("test"), null, 2, 1));

    final PrologAtom testAtom = new PrologAtom("test");
    final TokenizerResult result = new TokenizerResult(testAtom,
        TokenizerState.ATOM, 2, 1);
    assertSame(testAtom, result.getResult());
    assertEquals(TokenizerState.ATOM, result.getTokenizerState());

  }

  @Test
  public void testGetTokenizerState() {
    final PrologAtom testAtom = new PrologAtom("test");
    final TokenizerResult result = new TokenizerResult(testAtom,
        TokenizerState.STRING, 2, 1);
    assertSame(testAtom, result.getResult());
    assertEquals(TokenizerState.STRING, result.getTokenizerState());
  }

  @Test
  public void testGetResult() {
    final PrologInt testAtom = new PrologInt("322323423");
    final TokenizerResult result = new TokenizerResult(testAtom,
        TokenizerState.LOOK_FOR, 2, 1);
    assertSame(testAtom, result.getResult());
    assertEquals(TokenizerState.LOOK_FOR, result.getTokenizerState());
  }

  @Test
  public void testGetTermType() {
    final PrologInt testAtom = new PrologInt("322323423");
    final TokenizerResult result = new TokenizerResult(testAtom,
        TokenizerState.LOOK_FOR, 2, 1);
    assertSame(testAtom, result.getResult());
    assertEquals(TermType.ATOM, result.getResult().getType());
  }

  @Test
  public void testGetStringPosition() {
    final TokenizerResult result = new TokenizerResult(new PrologAtom("test"), TokenizerState.LOOK_FOR, 2, 1);
    assertEquals(1, result.getPos());
  }

  @Test
  public void testGetLineNumber() {
    final TokenizerResult result = new TokenizerResult(new PrologAtom("test"), TokenizerState.LOOK_FOR, 2, 1);
    assertEquals(2, result.getLine());
  }
}
