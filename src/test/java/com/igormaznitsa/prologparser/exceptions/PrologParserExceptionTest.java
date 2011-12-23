package com.igormaznitsa.prologparser.exceptions;

import static org.junit.Assert.*;
import org.junit.Test;
import com.igormaznitsa.prologparser.AbstractPrologParserTest;

public class PrologParserExceptionTest extends AbstractPrologParserTest {

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
            assertEquals(110, ex.getLineNumber());
        }
    }

    @Test
    public void testGetStringPosition() {
        try {
            throw new PrologParserException("Hello world", 110, 32);
        } catch (PrologParserException ex) {
            assertEquals(32, ex.getStringPosition());
        }
    }

    @Test
    public void testContainsRightPositionData() {
        try {
            throw new PrologParserException("Hello world", -1, 0);
        } catch (PrologParserException ex) {
            assertFalse(ex.containsRightPositionData());
        }

        try {
            throw new PrologParserException("Hello world", -1, -1);
        } catch (PrologParserException ex) {
            assertFalse(ex.containsRightPositionData());
        }

        try {
            throw new PrologParserException("Hello world", 0, 0);
        } catch (PrologParserException ex) {
            assertFalse(ex.containsRightPositionData());
        }

        try {
            throw new PrologParserException("Hello world", 12, -1);
        } catch (PrologParserException ex) {
            assertFalse(ex.containsRightPositionData());
        }

        try {
            throw new PrologParserException("Hello world", 1, 10);
        } catch (PrologParserException ex) {
            assertTrue(ex.containsRightPositionData());
        }
    }

    @Test
    public void testToString() {
        assertEquals("Hello World[1:10]", new PrologParserException("Hello World", 1, 10).toString());
    }
}
