package com.igormaznitsa.prologparser.utils;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;
import com.igormaznitsa.prologparser.utils.StringUtils.Mutable;

public class StringUtilsTest extends AbstractPrologParserTest {

    @Test
    public void testUnescapeCharacter() {

        final Mutable<Character> container = new Mutable<Character>(null);

        try {
            StringUtils.unescapeCharacter("Test", null);
        } catch (NullPointerException ex) {
        }

        assertFalse(StringUtils.unescapeCharacter(null, container));
        assertNull(container.get());

        assertTrue(StringUtils.unescapeCharacter("a", container));
        assertEquals(Character.valueOf((char) 7), container.get());

        assertTrue(StringUtils.unescapeCharacter("b", container));
        assertEquals(Character.valueOf((char) 8), container.get());

        assertTrue(StringUtils.unescapeCharacter("e", container));
        assertEquals(Character.valueOf((char) 27), container.get());

        assertTrue(StringUtils.unescapeCharacter("r", container));
        assertEquals(Character.valueOf('\r'), container.get());

        assertTrue(StringUtils.unescapeCharacter("n", container));
        assertEquals(Character.valueOf('\n'), container.get());

        assertTrue(StringUtils.unescapeCharacter("\\", container));
        assertEquals(Character.valueOf('\\'), container.get());

        assertTrue(StringUtils.unescapeCharacter("\'", container));
        assertEquals(Character.valueOf('\''), container.get());

        assertTrue(StringUtils.unescapeCharacter("t", container));
        assertEquals(Character.valueOf('\t'), container.get());

        assertFalse(StringUtils.unescapeCharacter("z", container));
        assertNull(container.get());

        assertTrue(StringUtils.unescapeCharacter("xa", container));
        assertNull(container.get());

        assertTrue(StringUtils.unescapeCharacter("s", container));
        assertEquals(Character.valueOf(' '), container.get());

        assertTrue(StringUtils.unescapeCharacter("v", container));
        assertEquals(Character.valueOf((char) 11), container.get());

        assertFalse(StringUtils.unescapeCharacter("xm", container));
        assertNull(container.get());

        assertFalse(StringUtils.unescapeCharacter("x12234", container));
        assertNull(container.get());

        assertTrue(StringUtils.unescapeCharacter("x20", container));
        assertEquals(Character.valueOf((char) 32), container.get());

        assertFalse(StringUtils.unescapeCharacter("x2023", container));
        assertNull(container.get());

        assertFalse(StringUtils.unescapeCharacter("Xff", container));
        assertNull(container.get());

        assertTrue(StringUtils.unescapeCharacter("uFF0", container));
        assertNull(container.get());

        assertTrue(StringUtils.unescapeCharacter("uFF00", container));
        assertEquals(Character.valueOf((char) 0xFF00), container.get());

        assertTrue(StringUtils.unescapeCharacter("uFF0a", container));
        assertEquals(Character.valueOf((char) 0xFF0A), container.get());

        assertTrue(StringUtils.unescapeCharacter("ufF0A", container));
        assertEquals(Character.valueOf((char) 0xFF0A), container.get());

        assertFalse(StringUtils.unescapeCharacter("ufF0AC2", container));
        assertNull(container.get());

        assertFalse(StringUtils.unescapeCharacter("uFF0z", container));
        assertNull(container.get());

        assertTrue(StringUtils.unescapeCharacter("ubbbb", container));
        assertEquals(Character.valueOf((char) 0xBBBB), container.get());

        assertFalse(StringUtils.unescapeCharacter("Ubbbb", container));
        assertNull(container.get());
    }

    @Test
    public void testEscapeString() {
        final String test = "Hello\r\'World\'\nAnd Skolkovo too\t\u0007\u001b\u000b";

        assertEquals(StringUtils.escapeString(test), "Hello\\r\\'World\\'\\nAnd Skolkovo too\\t\\a\\e\\v");
    }
}
