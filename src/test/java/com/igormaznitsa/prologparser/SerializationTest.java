package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.terms.PrologStructure;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import static org.junit.jupiter.api.Assertions.*;

public class SerializationTest {
  @Test
  public void testSerializationOperator() throws Exception {
    final GenericPrologParser parser = new EdinburghPrologParser(null);

    final CharSource source = CharSource.of("a+b. c+d.");

    final PrologStructure first = (PrologStructure) parser.nextSentence(source);
    final PrologStructure second = (PrologStructure) parser.nextSentence(source);

    assertNotNull(first);
    assertNotNull(second);
    assertNotSame(first, second);

    assertSame(first.getFunctor(), second.getFunctor());

    final ByteArrayOutputStream buffer = new ByteArrayOutputStream(16384);
    final ObjectOutputStream objectStream = new ObjectOutputStream(buffer);

    objectStream.writeObject(first);
    objectStream.writeObject(second);
    objectStream.close();

    final ObjectInputStream inStream = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray()));

    final PrologStructure firstClone = (PrologStructure) inStream.readObject();
    final PrologStructure secondClone = (PrologStructure) inStream.readObject();

    assertNotSame(first, second);
    assertSame(firstClone.getFunctor(), secondClone.getFunctor());
    assertSame(first.getFunctor(), firstClone.getFunctor());
    assertSame(second.getFunctor(), secondClone.getFunctor());
  }

  @Test
  public void testSerializationOfVariables() throws Exception {
    final GenericPrologParser parser = new EdinburghPrologParser(null);
    final PrologStructure structure = (PrologStructure) parser.nextSentence("a(A,A).");

    assertNotSame(structure.getElement(0), structure.getElement(1));

    final ByteArrayOutputStream buffer = new ByteArrayOutputStream(16384);
    final ObjectOutputStream objectStream = new ObjectOutputStream(buffer);

    objectStream.writeObject(structure);
    objectStream.close();

    final ObjectInputStream inStream = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray()));

    final PrologStructure structureClone = (PrologStructure) inStream.readObject();

    assertNotSame(structure, structureClone);
    assertNotSame(structureClone.getElement(0), structureClone.getElement(1));

  }
}
