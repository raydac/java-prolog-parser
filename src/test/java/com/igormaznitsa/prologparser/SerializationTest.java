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

import com.igormaznitsa.prologparser.terms.PrologStructure;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import static org.junit.Assert.*;
import org.junit.Test;

public class SerializationTest {
    @Test
    public void testSerializationOperator() throws Exception {
        final PrologParser parser = new PrologParser(null);
        
        final PrologCharDataSource source = new PrologCharDataSource("a+b. c+d.");
        
        final PrologStructure first = (PrologStructure)parser.nextSentence(source);
        final PrologStructure second = (PrologStructure)parser.nextSentence(source);
        
        assertNotNull(first);
        assertNotNull(second);
        assertNotSame(first,second);
        
        assertSame("Must be the same", first.getFunctor(), second.getFunctor());
     
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream(16384);
        final ObjectOutputStream objectStream = new ObjectOutputStream(buffer);
        
        objectStream.writeObject(first);
        objectStream.writeObject(second);
        objectStream.close();
        
        final ObjectInputStream inStream = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray()));
        
        final PrologStructure firstClone = (PrologStructure)inStream.readObject();
        final PrologStructure secondClone = (PrologStructure)inStream.readObject();
        
        assertNotSame(first, second);
        assertSame("Must be the same", firstClone.getFunctor(), secondClone.getFunctor());
        assertSame("Must be the same", first.getFunctor(), firstClone.getFunctor());
        assertSame("Must be the same", second.getFunctor(), secondClone.getFunctor());
    }
    
    @Test
    public void testSerializationOfVariables() throws Exception {
        final PrologParser parser = new PrologParser(null);
        final PrologStructure structure = (PrologStructure) parser.nextSentence("a(A,A).");
        
        assertNotSame("Must not be the same", structure.getElement(0), structure.getElement(1));

        final ByteArrayOutputStream buffer = new ByteArrayOutputStream(16384);
        final ObjectOutputStream objectStream = new ObjectOutputStream(buffer);

        objectStream.writeObject(structure);
        objectStream.close();

        final ObjectInputStream inStream = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray()));

        final PrologStructure structureClone = (PrologStructure) inStream.readObject();

        assertNotSame("Must not be the same", structure, structureClone);
        assertNotSame("Must not be the same", structureClone.getElement(0), structureClone.getElement(1));
        
    }
}
