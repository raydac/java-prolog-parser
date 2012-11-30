package com.igormaznitsa.prologparser;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;
import static org.mockito.Mockito.*;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.operators.OperatorType;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;

public class ParserContextTest extends AbstractPrologParserTest {

    @Test
    public void testHasOperatorStartsWith() throws Exception {
        final ParserContext mockContext = mock(ParserContext.class);
        stub(mockContext.hasOperatorStartsWith(any(PrologParser.class), anyString())).toAnswer(new Answer<Boolean>() {
            @Override
            public Boolean answer(InvocationOnMock invocation) throws Throwable {
                return "operator".startsWith((String) invocation.getArguments()[1]);
            }
        });


        final PrologParser parser = new PrologParser(mockContext);
        final PrologCharDataSource reader = new PrologCharDataSource("a operator b.");
        try {
            parser.nextSentence(reader);
            fail("Must throw PPE");
        } catch (PrologParserException ex) {
        }

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
        stub(mockContext.findOperatorForName(any(PrologParser.class), anyString())).toAnswer(new Answer<OperatorContainer>() {
            @Override
            public OperatorContainer answer(InvocationOnMock invocation) throws Throwable {
                if ("operator".startsWith((String) invocation.getArguments()[1])) {
                    return new OperatorContainer(Operator.makeOperator(1000, OperatorType.XFX, "operator"));
                } else {
                    return null;
                }
            }
        });

        stub(mockContext.hasOperatorStartsWith(any(PrologParser.class), anyString())).toAnswer(new Answer<Boolean>() {
            @Override
            public Boolean answer(InvocationOnMock invocation) throws Throwable {
                return "operator".startsWith((String) invocation.getArguments()[1]);
            }
        });

        final PrologParser parser = new PrologParser(mockContext);
        final PrologCharDataSource reader = new PrologCharDataSource("operator.");
        try {
            parser.nextSentence(reader);
            fail("Must throw PPE");
        } catch (PrologParserException ex) {
        }

        verify(mockContext).findOperatorForName(parser, "operator");

    }

    @Test
    public void testHasZeroArityPredicate() throws Exception {
        final ParserContext mockContext = mock(ParserContext.class);
        stub(mockContext.hasZeroArityPredicate(any(PrologParser.class), anyString())).toAnswer(new Answer<Boolean>() {
            @Override
            public Boolean answer(InvocationOnMock invocation) throws Throwable {
                return "foo".equals(invocation.getArguments()[1]);
            }
        });

        final PrologParser parser = new PrologParser(mockContext);

        final AbstractPrologTerm term = parser.nextSentence("foo.");
        assertNotNull(term);
        assertEquals(PrologTermType.STRUCT, term.getType());
        assertEquals(0, ((PrologStructure) term).getArity());
        assertEquals("foo", term.getText());
        assertNull(parser.nextSentence());

        verify(mockContext).hasZeroArityPredicate(parser, "foo");
    }

    @Test
    public void testProcessNewStructure() throws Exception {
        final Map<String, PrologStructure> detectedStructures = new HashMap<String, PrologStructure>();
        final ParserContext stubContext = new ParserContext() {
            @Override
            public void processNewStructure(final PrologParser source, final PrologStructure structure) {
                detectedStructures.put(structure.getFunctor().getText(), structure);
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
            public OperatorContainer findOperatorForName(final PrologParser source, String operatorName) {
                return null;
            }
        };

        final PrologParser parser = new PrologParser(stubContext);
        final PrologCharDataSource reader = new PrologCharDataSource("test(1,2,3).foo.ttt(5). a :- b.");
        while (parser.nextSentence(reader) != null);

        assertEquals(4, detectedStructures.size());
        assertTrue(detectedStructures.containsKey("test"));
        assertTrue(detectedStructures.containsKey("foo"));
        assertTrue(detectedStructures.containsKey("ttt"));
        assertTrue(detectedStructures.containsKey(":-"));

    }
}
