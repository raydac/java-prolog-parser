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
package com.igormaznitsa.prologparser;

import static org.junit.Assert.*;

import org.junit.Test;

import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologTermType;

public class TokenizerResultTest {

    @Test
    public void testTokenizerResult() {
        try {
            new TokenizerResult(null, TokenizerState.ATOM, 1, 2);
            fail("Must throw NPE for null term");
        } catch (NullPointerException ex) {
        }

        try {
            new TokenizerResult(new PrologAtom("test"), null, 1, 2);
            fail("Must throw NPE for null state");
        } catch (NullPointerException ex) {
        }

        final PrologAtom testAtom = new PrologAtom("test");
        final TokenizerResult result = new TokenizerResult(testAtom,
                TokenizerState.ATOM, 1, 2);
        assertSame(testAtom, result.getResult());
        assertEquals(TokenizerState.ATOM, result.getTokenizerState());

    }

    @Test
    public void testGetTokenizerState() {
        final PrologAtom testAtom = new PrologAtom("test");
        final TokenizerResult result = new TokenizerResult(testAtom,
                TokenizerState.STRING, 1, 2);
        assertSame(testAtom, result.getResult());
        assertEquals(TokenizerState.STRING, result.getTokenizerState());
    }

    @Test
    public void testGetResult() {
        final PrologIntegerNumber testAtom = new PrologIntegerNumber("322323423");
        final TokenizerResult result = new TokenizerResult(testAtom,
                TokenizerState.LOOKFOR, 1, 2);
        assertSame(testAtom, result.getResult());
        assertEquals(TokenizerState.LOOKFOR, result.getTokenizerState());
    }

    @Test
    public void testGetTermType() {
        final PrologIntegerNumber testAtom = new PrologIntegerNumber("322323423");
        final TokenizerResult result = new TokenizerResult(testAtom,
                TokenizerState.LOOKFOR, 1, 2);
        assertSame(testAtom, result.getResult());
        assertEquals(PrologTermType.ATOM, result.getTermType());
    }

    @Test
    public void testGetStringPosition() {
        final TokenizerResult result = new TokenizerResult(new PrologAtom("test"), TokenizerState.LOOKFOR, 1, 2);
        assertEquals(1, result.getStringPosition());
    }

    @Test
    public void testGetLineNumber() {
        final TokenizerResult result = new TokenizerResult(new PrologAtom("test"), TokenizerState.LOOKFOR, 1, 2);
        assertEquals(2, result.getLineNumber());
    }
}
