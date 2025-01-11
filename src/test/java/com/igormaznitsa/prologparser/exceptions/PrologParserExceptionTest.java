package com.igormaznitsa.prologparser.exceptions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.prologparser.DefaultParserContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.TokenizerResult;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

public class PrologParserExceptionTest {

  @Test
  public void testPrologParserException() {
    try {
      throw new PrologParserException("Hello World", 110, 32);
    } catch (PrologParserException ex) {
      assertEquals("Hello World", ex.getMessage());
    }
  }

  @Test
  public void testGetLineNumber() {
    try {
      throw new PrologParserException("Hello world", 110, 32);
    } catch (PrologParserException ex) {
      assertEquals(110, ex.getLine());
    }
  }

  @Test
  public void testGetStringPosition() {
    try {
      throw new PrologParserException("Hello world", 110, 32);
    } catch (PrologParserException ex) {
      assertEquals(32, ex.getPos());
    }
  }

  @Test
  public void testContainsRightPositionData() {
    try {
      throw new PrologParserException("Hello world", -1, 0);
    } catch (PrologParserException ex) {
      assertFalse(ex.hasValidPosition());
    }

    try {
      throw new PrologParserException("Hello world", -1, -1);
    } catch (PrologParserException ex) {
      assertFalse(ex.hasValidPosition());
    }

    try {
      throw new PrologParserException("Hello world", 0, 0);
    } catch (PrologParserException ex) {
      assertFalse(ex.hasValidPosition());
    }

    try {
      throw new PrologParserException("Hello world", 12, -1);
    } catch (PrologParserException ex) {
      assertFalse(ex.hasValidPosition());
    }

    try {
      throw new PrologParserException("Hello world", 1, 10);
    } catch (PrologParserException ex) {
      assertTrue(ex.hasValidPosition());
    }
  }

  @Test
  public void testToString() {
    assertEquals("Hello World[1:10]", new PrologParserException("Hello World", 1, 10).toString());
  }

  @Test
  public void testNotificationAboutComments() throws Exception {
    final List<TokenizerResult> foundComments = new ArrayList<>();
    final List<PrologTerm> foundTerms = new ArrayList<>();

    try (final PrologParser parser = new GenericPrologParser(
        new StringReader("% Line1\nhello(/*HHH*/world/*End*/).% Ending comment\n[1,2,3]."),
        DefaultParserContext.of(
            ParserContext.FLAG_BLOCK_COMMENTS | ParserContext.FLAG_COMMENTS_AWARENESS, Op.ISO),
        List.of((parser1, comment) -> foundComments.add(comment))
    )) {
      parser.forEach(foundTerms::add);
    }

    assertEquals(4, foundComments.size());
    assertEquals(" Line1", foundComments.get(0).getResult().getText());
    assertEquals("HHH", foundComments.get(1).getResult().getText());
    assertEquals("End", foundComments.get(2).getResult().getText());
    assertEquals(" Ending comment", foundComments.get(3).getResult().getText());

    assertEquals(2, foundTerms.size());
    assertEquals("hello(world)", foundTerms.get(0).toString());
    assertEquals("[1, 2, 3]", foundTerms.get(1).toString());

  }
}
