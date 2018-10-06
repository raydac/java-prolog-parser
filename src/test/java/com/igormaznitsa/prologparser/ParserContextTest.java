package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Op;
import com.igormaznitsa.prologparser.operators.OpContainer;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.PrologParser;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;

import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

public class ParserContextTest {

  @Test
  public void testHasOperatorStartsWith() {
    final ParserContext mockContext = mock(ParserContext.class);
    Mockito.when(mockContext.hasOperatorStartsWith(any(GenericPrologParser.class), anyString())).then((InvocationOnMock invocation) -> "operator".startsWith((String) invocation.getArguments()[1]));

    final GenericPrologParser parser = new EdinburghPrologParser(new StringReader("a operator b."), mockContext);

    assertThrows(PrologParserException.class, () -> parser.next());

    verify(mockContext).hasOperatorStartsWith(parser, "a");
    verify(mockContext).hasOperatorStartsWith(parser, "o");
    verify(mockContext).hasOperatorStartsWith(parser, "op");
    verify(mockContext).hasOperatorStartsWith(parser, "ope");
    verify(mockContext).hasOperatorStartsWith(parser, "oper");
    verify(mockContext).hasOperatorStartsWith(parser, "opera");
    verify(mockContext).hasOperatorStartsWith(parser, "operato");
    verify(mockContext).hasOperatorStartsWith(parser, "operator");
    verify(mockContext).hasOperatorStartsWith(parser, "b");
  }

  @Test
  public void testFindOperatorForName() throws Exception {
    final ParserContext mockContext = mock(ParserContext.class);
    when(mockContext.findOperatorForName(any(GenericPrologParser.class), anyString())).then((InvocationOnMock invocation) -> {
      if ("operator".startsWith((String) invocation.getArguments()[1])) {
        return OpContainer.newOpCont(Op.makeOne(1000, OpType.XFX, "operator"));
      } else {
        return null;
      }
    });

    Mockito.when(mockContext.hasOperatorStartsWith(any(GenericPrologParser.class), anyString())).then((InvocationOnMock invocation) -> "operator".startsWith((String) invocation.getArguments()[1]));

    final GenericPrologParser parser = new EdinburghPrologParser(new StringReader("operator."), mockContext);
    final PrologAtom atom = (PrologAtom) parser.next();
    assertEquals("operator", atom.getText());

    verify(mockContext).findOperatorForName(parser, "operator");

  }

  @Test
  public void testHasZeroArityPredicate() throws Exception {
    final ParserContext mockContext = mock(ParserContext.class);
    Mockito.when(mockContext.hasZeroArityPredicate(any(GenericPrologParser.class), anyString())).then((InvocationOnMock invocation) -> "foo".equals(invocation.getArguments()[1]));

    final GenericPrologParser parser = new EdinburghPrologParser(new StringReader("foo."), mockContext);

    final PrologTerm term = parser.next();
    assertNotNull(term);
    assertEquals(TermType.STRUCT, term.getType());
    assertEquals(0, ((PrologStruct) term).getArity());
    assertEquals("foo", term.getText());
    assertFalse(parser.hasNext());

    verify(mockContext).hasZeroArityPredicate(parser, "foo");
  }

  @Test
  public void testProcessNewStructure() {
    final Map<String, PrologStruct> detectedStructures = new HashMap<>();
    final ParserContext stubContext = new ParserContext() {
      @Override
      public void onStructureCreated(final PrologParser source, final PrologStruct struct) {
        detectedStructures.put(struct.getFunctor().getText(), struct);
      }

      @Override
      public boolean hasZeroArityPredicate(final PrologParser source, String predicateName) {
        return "foo".equals(predicateName);
      }

      @Override
      public boolean hasOperatorStartsWith(final PrologParser source, String operatorNameStartSubstring) {
        return false;
      }

      @Override
      public OpContainer findOperatorForName(final PrologParser source, String operatorName) {
        return null;
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
