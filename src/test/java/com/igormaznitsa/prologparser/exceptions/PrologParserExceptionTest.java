/* 
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
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
package com.igormaznitsa.prologparser.exceptions;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;
import org.junit.Test;

import static org.junit.Assert.*;

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
