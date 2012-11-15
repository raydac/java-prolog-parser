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
    public void testPrologAtom_String_NPE() {
        try {
            new PrologAtom((String)null);
            fail("Null name must throw NPE");
        } catch (NullPointerException ex) {
        }
    }

    @Test
    public void testPrologAtom_Term_NPE() {
        try {
            new PrologAtom((AbstractPrologTerm)null);
            fail("Null name must throw NPE");
        } catch (NullPointerException ex) {
        }
    }

    @Test
    public void testPrologAtom_String(){
        final PrologAtom atom = new PrologAtom("test");
        assertEquals("Must be 'test'","test",atom.getText());
    }
    
    @Test
    public void testPrologAtom_Term(){
        final PrologAtom etalon = new PrologAtom("etal", 111, 222);
        
        final PrologAtom atom = new PrologAtom(etalon);
        assertEquals("Must be 'etal'","etal",atom.getText());
        assertEquals("Must be 111",111,atom.getStrPosition());
        assertEquals("Must be 222",222,atom.getLineNumber());
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
