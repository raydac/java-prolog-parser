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
    public void testPrologAtomString() {
        try {
            new PrologAtom(null);
            fail("Null name must throw NPE");
        } catch (NullPointerException ex) {
        }

        new PrologAtom("Hello");
    }

    @Test
    public void testPrologAtomStringIntInt() {
        try {
            new PrologAtom(null, 0, 0);
            fail("Must throw NPE for null name");
        } catch (NullPointerException ex) {
        }

        final AbstractPrologTerm term = new PrologAtom("test", 1, 2);
        assertEquals(1, term.getStrPosition());
        assertEquals(2, term.getLineNumber());
    }
}
