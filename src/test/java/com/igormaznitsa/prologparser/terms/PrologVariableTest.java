package com.igormaznitsa.prologparser.terms;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;

public class PrologVariableTest extends AbstractPrologParserTest {

    @Test
    public void testGetType() {
        final PrologVariable var = new PrologVariable("X");
        assertEquals(PrologTermType.VAR, var.getType());

        final PrologVariable var2 = new PrologVariable();
        assertEquals(PrologTermType.VAR, var2.getType());
    }

    @Test
    public void testPrologVariable() {
        final PrologVariable var = new PrologVariable();
        assertTrue(var.isAnonymous());
    }

    @Test
    public void testPrologVariableIntInt() {
        final PrologVariable var = new PrologVariable(1, 2);
        assertTrue(var.isAnonymous());
        assertEquals(1, var.getStrPosition());
        assertEquals(2, var.getLineNumber());
    }

    @Test
    public void testPrologVariableString() {
        try {
            new PrologVariable(null);
            fail("The null argument must throw NPE");
        } catch (NullPointerException ex) {
        }

        try {
            new PrologVariable("");
            fail("Must throw IAE for wrong prolog variable name");
        } catch (IllegalArgumentException ex) {
        }

        try {
            new PrologVariable("привет");
            fail("Must throw IAE for wrong prolog variable name");
        } catch (IllegalArgumentException ex) {
        }

        try {
            new PrologVariable("abc");
            fail("Must throw IAE for wrong prolog variable name");
        } catch (IllegalArgumentException ex) {
        }

        PrologVariable var = new PrologVariable("X");
        assertFalse(var.isAnonymous());
        assertEquals("X", var.getText());

        var = new PrologVariable("_");
        assertTrue(var.isAnonymous());
        assertEquals("_", var.getText());

        var = new PrologVariable("_hello_world");
        assertFalse(var.isAnonymous());
        assertEquals("_hello_world", var.getText());

        var = new PrologVariable("Привет");
        assertFalse(var.isAnonymous());
        assertEquals("Привет", var.getText());
    }

    @Test
    public void testPrologVariableStringIntInt() {
        try {
            new PrologVariable(null, 1, 2);
            fail("The null argument must throw NPE");
        } catch (NullPointerException ex) {
        }

        try {
            new PrologVariable("", 1, 2);
            fail("Must throw IAE for wrong prolog variable name");
        } catch (IllegalArgumentException ex) {
        }

        try {
            new PrologVariable("привет", 1, 2);
            fail("Must throw IAE for wrong prolog variable name");
        } catch (IllegalArgumentException ex) {
        }

        try {
            new PrologVariable("abc", 1, 2);
            fail("Must throw IAE for wrong prolog variable name");
        } catch (IllegalArgumentException ex) {
        }

        PrologVariable var = new PrologVariable("X", 1, 2);

        assertEquals(1, var.getStrPosition());
        assertEquals(2, var.getLineNumber());
    }

    @Test
    public void testIsAnonymous() {
        PrologVariable var = new PrologVariable();
        assertTrue(var.isAnonymous());
        var = new PrologVariable("_");
        assertTrue(var.isAnonymous());
        var = new PrologVariable("Ddsd");
        assertFalse(var.isAnonymous());
    }

    @Test
    public void testGetText() {
        assertEquals("_", new PrologVariable().getText());
        assertEquals("_", new PrologVariable("_").getText());
        assertEquals("X", new PrologVariable("X").getText());
        assertEquals("Variable", new PrologVariable("Variable").getText());
    }

    @Test
    public void testGetPriority() {
        final PrologVariable var = new PrologVariable("Hello");
        assertEquals(0, var.getPriority());
    }

    @Test
    public void testToString() {
        assertEquals("_", new PrologVariable().toString());
        assertEquals("_", new PrologVariable("_").toString());
        assertEquals("__________test",
                new PrologVariable("__________test").toString());
        assertEquals("Abc", new PrologVariable("Abc").toString());
        assertEquals("Привет", new PrologVariable("Привет").toString());
    }

    @Test
    public void testGetLinkedVariable() {
        final PrologVariable var = new PrologVariable("Hello");
        final PrologVariable var2 = new PrologVariable("Hello");
        assertNull(var.getLinkedObject());
        var.setLinkedVariable(var2);
        assertSame(var2, var.getLinkedVariable());
    }

    @Test
    public void testSetLinkedVariable() {
        try {
            new PrologVariable().setLinkedVariable(new PrologVariable("Test"));
            fail("Must not accept linked variable for anonimous variables");
        } catch (UnsupportedOperationException ex) {
        }

        try {
            new PrologVariable("Test").setLinkedVariable(new PrologVariable("Test2"));
            fail("Must not support linked variable with different name");
        } catch (IllegalArgumentException ex) {
        }

        final PrologVariable var = new PrologVariable("Hello");
        final PrologVariable var2 = new PrologVariable("Hello");
        assertNull(var.getLinkedObject());
        var.setLinkedVariable(var2);
        assertSame(var2, var.getLinkedVariable());
        var.setLinkedVariable(null);
        assertNull(var.getLinkedObject());
    }
}
