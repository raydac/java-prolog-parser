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
package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

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
