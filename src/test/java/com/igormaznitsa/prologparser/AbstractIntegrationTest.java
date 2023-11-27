/*
 * Copyright 2019 Igor Maznitsa.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.igormaznitsa.prologparser;

import static com.igormaznitsa.prologparser.DefaultParserContext.of;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_BLOCK_COMMENTS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_CURLY_BRACKETS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_DOT2_AS_LIST;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_ZERO_QUOTATION_CHARCODE;
import static com.igormaznitsa.prologparser.terms.TermType.ATOM;
import static java.util.Objects.requireNonNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologInt;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

abstract class AbstractIntegrationTest {
  public PrologParser parseCpl(final String str) {
    return new GenericPrologParser(new StringReader(str), DefaultParserContext.of(
        FLAG_BLOCK_COMMENTS | FLAG_ZERO_QUOTATION_CHARCODE | FLAG_CURLY_BRACKETS, Op.SWI,
        Op.SWI_CPL));
  }

  public PrologParser parseIso(final String str) {
    return new GenericPrologParser(new StringReader(str),
        DefaultParserContext.of(FLAG_ZERO_QUOTATION_CHARCODE | FLAG_DOT2_AS_LIST, Op.ISO));
  }

  public PrologParser parseEd(final String str) {
    final ParserContext parserContext = mock(ParserContext.class);
    when(parserContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    when(parserContext.getFlags()).thenReturn(
        FLAG_BLOCK_COMMENTS | FLAG_ZERO_QUOTATION_CHARCODE | FLAG_CURLY_BRACKETS);
    return parseEd(str, parserContext);
  }

  public PrologParser parseEd(final String str, final int parseFlags) {
    final ParserContext parserContext = mock(ParserContext.class);
    when(parserContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    when(parserContext.getFlags()).thenReturn(parseFlags);
    return parseEd(str, parserContext);
  }

  public PrologParser parseEd(final String str, final ParserContext context) {
    return parseEd(new StringReader(str), context);
  }

  public PrologParser parseEd(final Reader reader, final ParserContext context) {
    return new GenericPrologParser(reader, ParserContextChain.of(
        DefaultParserContext.of(
            FLAG_BLOCK_COMMENTS | FLAG_ZERO_QUOTATION_CHARCODE | FLAG_CURLY_BRACKETS, Op.SWI),
        context));
  }

  public PrologParser parseGen(final String str, final ParserContext context) {
    final ParserContext parserContext = mock(ParserContext.class);
    when(parserContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    when(parserContext.getFlags()).thenReturn(
        ParserContext.FLAG_BLOCK_COMMENTS | FLAG_ZERO_QUOTATION_CHARCODE | FLAG_CURLY_BRACKETS);
    return new GenericPrologParser(new StringReader(str),
        ParserContextChain.of(context, parserContext));
  }

  public GenericPrologParser parseGen(final String str) {
    final ParserContext parserContext = mock(ParserContext.class);
    when(parserContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    when(parserContext.getFlags()).thenReturn(
        ParserContext.FLAG_BLOCK_COMMENTS | FLAG_ZERO_QUOTATION_CHARCODE);
    return new GenericPrologParser(new StringReader(str), parserContext);
  }

  public void assertReadTerms(final int expected, final String resource, final List<Op> ops) {
    assertReadTerms(expected, resource, ops.toArray(new Op[0]));
  }

  public void assertReadTerms(final int expected, final String resource, final Op... ops) {
    final ParserContext defaultContext =
        of(ParserContext.FLAG_BLOCK_COMMENTS | ParserContext.FLAG_ZERO_QUOTATION_CHARCODE |
            ParserContext.FLAG_ZERO_QUOTATION_ALLOWS_WHITESPACE_CHAR, ops);
    try (Reader reader = new InputStreamReader(
        requireNonNull(getClass().getResourceAsStream(resource)), StandardCharsets.UTF_8)) {
      final PrologParser parser = parseEd(reader, defaultContext);
      assertEquals(expected, parser.stream().count());
    } catch (IOException ex) {
      fail("IOException", ex);
    }
  }

  public ParserContext makeSictusContext(final Op... ops) {
    return of(ParserContext.FLAG_BLOCK_COMMENTS | ParserContext.FLAG_CURLY_BRACKETS, Op.ISO,
        Op.SICTUS_SPECIFIC, Arrays.asList(ops));
  }

  public void assertReadSictusTerms(final int expected, final String resource, final Op... ops) {
    final ParserContext defaultContext = makeSictusContext(ops);
    try (Reader reader = new InputStreamReader(
        requireNonNull(getClass().getResourceAsStream("bench/" + resource)),
        StandardCharsets.UTF_8)) {
      final PrologParser parser = new GenericPrologParser(reader, defaultContext);
      assertEquals(expected, parser.stream().count());
    } catch (IOException ex) {
      fail("IOException", ex);
    }
  }

  public void assertOperatorAsFunctor(final String goal, final String opText, final OpAssoc assoc,
                                      final int arity, final String expectedText) {
    try (final PrologParser parser = parseEd(goal)) {
      assertTrue(parser.hasNext());
      final PrologTerm term = parser.next();
      assertFalse(parser.hasNext());
      assertEquals(TermType.OPERATOR, term.getFunctor().getType(), term.toString());
      assertEquals(opText, term.getText(), term.toString());
      assertEquals(assoc, ((Op) term.getFunctor()).getAssoc(), term.toString());
      assertEquals(arity, term.getArity(), term.toString());
      assertEquals(expectedText, term.toString());
    } catch (IOException ex) {
      fail(ex);
    }
  }

  public void assertOperatorAsFunctor(final String expected, final PrologTerm term) {
    assertTrue(term instanceof PrologStruct);
    final PrologStruct struct = (PrologStruct) term;
    assertEquals(TermType.OPERATOR, struct.getFunctor().getType(), term.toString());
    assertEquals(struct.getFunctor().getArity(), struct.getArity(), term.toString());
    assertEquals(expected, term.toString(), term.toString());
  }

  public void checkIntegerWithoutPPE(final String atomToBeChecked, final long expectedNumber) {
    try (final PrologParser parser = parseEd(atomToBeChecked + '.')) {
      PrologTerm atom = parser.next();
      assertEquals(ATOM, atom.getType(), "Type: " + atom.getType());
      assertEquals(PrologInt.class, atom.getClass(), "Class: " + atom.getClass());
      assertEquals(expectedNumber, ((PrologInt) atom).getNumber().longValue(),
          "Number: " + ((PrologInt) atom).getNumber().longValue());
      assertEquals(Long.toString(expectedNumber), atom.getText(), "Text: " + atom.getText());
    } catch (IOException ex) {
      fail(ex);
    }
  }


  public static class StubContext implements ParserContext {

    private final Map<String, OpContainer> operators;

    public StubContext(final Map<String, OpContainer> operators) {
      this.operators = operators;
    }

    @Override
    public int getMaxTokenizerBufferLength() {
      return 1000;
    }

    @Override
    public int getFlags() {
      return FLAG_NONE;
    }

    @Override
    public boolean hasOpStartsWith(final PrologParser source,
                                   final String operatorNameStartSubstring) {
      for (final String string : operators.keySet()) {
        if (string.startsWith(operatorNameStartSubstring)) {
          return true;
        }
      }

      return false;
    }

    @Override
    public OpContainer findOpForName(final PrologParser source,
                                     final String operatorName) {
      return operators.get(operatorName);
    }

  }
}
