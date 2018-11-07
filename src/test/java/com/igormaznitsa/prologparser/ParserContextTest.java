package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;

import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

public class ParserContextTest {

  @Test
  public void testHasOperatorStartsWith() {
    final ParserContext mockContext = mock(ParserContext.class);
    Mockito.when(mockContext
        .hasOpStartsWith(any(GenericPrologParser.class), anyString()))
        .then((InvocationOnMock invocation) -> "operator".startsWith((String) invocation.getArguments()[1]));

    when(mockContext.getMaxTokenizerBufferLength()).thenReturn(1024);
    final GenericPrologParser parser = new GenericPrologParser(new StringReader("a operator b."), mockContext);

    assertThrows(PrologParserException.class, parser::next);

    verify(mockContext).findOpForName(parser, "a");
    verify(mockContext).findOpForName(parser, "operator");
    verify(mockContext).findOpForName(parser, "b");
    verify(mockContext, never()).hasOpStartsWith(parser, eq(anyString()));
  }

  @Test
  public void testFindOperatorForName() {
    final ParserContext mockContext = mock(ParserContext.class);
    when(mockContext.findOpForName(any(GenericPrologParser.class), anyString())).then((InvocationOnMock invocation) -> {
      if ("operator".startsWith((String) invocation.getArguments()[1])) {
        return OpContainer.make(Op.make(1000, OpAssoc.XFX, "operator"));
      } else {
        return null;
      }
    });

    Mockito.when(mockContext.hasOpStartsWith(any(GenericPrologParser.class), anyString())).then((InvocationOnMock invocation) -> "operator".startsWith((String) invocation.getArguments()[1]));
    when(mockContext.getMaxTokenizerBufferLength()).thenReturn(1024);

    final GenericPrologParser parser = new GenericPrologParser(new StringReader("operator."), mockContext);
    final PrologAtom atom = (PrologAtom) parser.next();
    assertEquals("operator", atom.getText());

    verify(mockContext).findOpForName(parser, "operator");

  }

}
