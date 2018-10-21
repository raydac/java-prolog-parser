package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.AbstractPrologParser;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpType;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;

import java.io.StringReader;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static com.igormaznitsa.prologparser.DefaultParserContext.of;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

public class ParserContextTest {

  @Test
  public void testHasOperatorStartsWith() {
    final ParserContext mockContext = mock(ParserContext.class);
    Mockito.when(mockContext
        .hasOpStartsWith(any(GenericPrologParser.class), anyString()))
        .then((InvocationOnMock invocation) -> "operator".startsWith((String) invocation.getArguments()[1]));

    final GenericPrologParser parser = new GenericPrologParser(new StringReader("a operator b."), mockContext);

    assertThrows(PrologParserException.class, () -> parser.next());

    verify(mockContext).findOpForName(parser, "a");
    verify(mockContext).findOpForName(parser, "operator");
    verify(mockContext).findOpForName(parser, "b");
    verify(mockContext, never()).hasOpStartsWith(parser, eq(anyString()));
  }

  @Test
  public void testFindOperatorForName() throws Exception {
    final ParserContext mockContext = mock(ParserContext.class);
    when(mockContext.findOpForName(any(GenericPrologParser.class), anyString())).then((InvocationOnMock invocation) -> {
      if ("operator".startsWith((String) invocation.getArguments()[1])) {
        return OpContainer.make(Op.make(1000, OpType.XFX, "operator"));
      } else {
        return null;
      }
    });

    Mockito.when(mockContext.hasOpStartsWith(any(GenericPrologParser.class), anyString())).then((InvocationOnMock invocation) -> "operator".startsWith((String) invocation.getArguments()[1]));

    final GenericPrologParser parser = new EdinburghPrologParser(new StringReader("operator."), mockContext);
    final PrologAtom atom = (PrologAtom) parser.next();
    assertEquals("operator", atom.getTermText());

    verify(mockContext).findOpForName(parser, "operator");

  }

  @Test
  public void testHasZeroArityStruct() {
    final ParserContext context = of(ParserContext.FLAG_BLOCK_COMMENTS, "foo");

    final GenericPrologParser parser = new EdinburghPrologParser(new StringReader("foo.faa."), context);

    PrologTerm term = parser.next();
    assertNotNull(term);
    assertEquals(TermType.STRUCT, term.getTermType());
    assertEquals(0, ((PrologStruct) term).getArity());
    assertEquals("foo", term.getTermText());
    assertTrue(parser.hasNext());

    term = parser.next();
    assertNotNull(term);
    assertEquals(TermType.ATOM, term.getTermType());
    assertEquals("faa", term.getTermText());

    assertFalse(parser.hasNext());
  }

  @Test
  public void testProcessNewStructure() {
    final Map<String, PrologStruct> detectedStructures = new HashMap<>();
    final ParserContext stubContext = new ParserContext() {
      @Override
      public Map<String, OpContainer> findAllOperators() {
        return Collections.emptyMap();
      }

      @Override
      public void onNewStruct(final AbstractPrologParser source, final PrologStruct struct) {
        detectedStructures.put(struct.getFunctor().getTermText(), struct);
      }

      @Override
      public boolean hasZeroStruct(final AbstractPrologParser source, String atomName) {
        return "foo".equals(atomName);
      }

      @Override
      public boolean hasOpStartsWith(final AbstractPrologParser source, String operatorNameStartSubstring) {
        return false;
      }

      @Override
      public OpContainer findOpForName(final AbstractPrologParser source, String operatorName) {
        return null;
      }

      @Override
      public int getFlags() {
        return ParserContext.FLAG_NONE;
      }

    };

    final GenericPrologParser parser = new EdinburghPrologParser(new StringReader("test(1,2,3).foo.ttt(5). a :- b."), stubContext);
    while (parser.hasNext()) {
      parser.next();
    }

    assertEquals(4, detectedStructures.size());
    assertTrue(detectedStructures.containsKey("test"));
    assertTrue(detectedStructures.containsKey("foo"));
    assertTrue(detectedStructures.containsKey("ttt"));
    assertTrue(detectedStructures.containsKey(":-"));

  }
}
