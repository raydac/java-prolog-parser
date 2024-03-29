package com.igormaznitsa.prologparser;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.igormaznitsa.prologparser.terms.PrologStruct;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.StringReader;
import org.junit.jupiter.api.Test;

public class SerializationTest {
  @Test
  public void testSerializationOperator() throws Exception {
    final GenericPrologParser parser = new GenericPrologParser(new StringReader("a,b. c,d."), null);

    final PrologStruct first = (PrologStruct) parser.next();
    final PrologStruct second = (PrologStruct) parser.next();

    assertNotNull(first);
    assertNotNull(second);
    assertNotSame(first, second);

    assertSame(first.getFunctor(), second.getFunctor());

    final ByteArrayOutputStream buffer = new ByteArrayOutputStream(16384);
    final ObjectOutputStream objectStream = new ObjectOutputStream(buffer);

    objectStream.writeObject(first);
    objectStream.writeObject(second);
    objectStream.close();

    final ObjectInputStream inStream =
        new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray()));

    final PrologStruct firstClone = (PrologStruct) inStream.readObject();
    final PrologStruct secondClone = (PrologStruct) inStream.readObject();

    assertNotSame(first, second);
    assertSame(firstClone.getFunctor(), secondClone.getFunctor());
    assertSame(first.getFunctor(), firstClone.getFunctor());
    assertSame(second.getFunctor(), secondClone.getFunctor());
  }

  @Test
  public void testSerializationOfVariables() throws Exception {
    final GenericPrologParser parser = new GenericPrologParser(new StringReader("a(A,A)."), null);
    final PrologStruct structure = (PrologStruct) parser.next();

    assertNotSame(structure.getTermAt(0), structure.getTermAt(1));

    final ByteArrayOutputStream buffer = new ByteArrayOutputStream(16384);
    final ObjectOutputStream objectStream = new ObjectOutputStream(buffer);

    objectStream.writeObject(structure);
    objectStream.close();

    final ObjectInputStream inStream =
        new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray()));

    final PrologStruct structureClone = (PrologStruct) inStream.readObject();

    assertNotSame(structure, structureClone);
    assertNotSame(structureClone.getTermAt(0), structureClone.getTermAt(1));

  }
}
