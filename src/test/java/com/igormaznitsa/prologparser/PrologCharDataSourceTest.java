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

import com.igormaznitsa.prologparser.utils.FastStringBuilder;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.Charset;

import static org.junit.jupiter.api.Assertions.*;

public class PrologCharDataSourceTest extends AbstractPrologParserTest {

  @Test
  public void testPrologCharDataSourceString() throws Exception {
    assertThrows(NullPointerException.class, () -> new PrologCharDataSource((String) null));

    final String testString = "It's a test string for prolog test. also there is UTF Привет";
    final PrologCharDataSource reader = new PrologCharDataSource(testString);
    for (final char chr : testString.toCharArray()) {
      assertEquals((int) chr, reader.read());
    }
    assertEquals(-1, reader.read());
  }

  @Test
  public void testPrologCharDataSourceInputStream() throws Exception {
    assertThrows(NullPointerException.class, () -> new PrologCharDataSource((InputStream) null));

    final String testString = "It's a test string for prolog test. also there is UTF Привет";
    final ByteArrayInputStream inStream = new ByteArrayInputStream(testString.getBytes(Charset.forName("UTF8")));
    final PrologCharDataSource reader = new PrologCharDataSource(inStream);

    for (final char chr : testString.toCharArray()) {
      assertEquals((int) chr, reader.read());
    }
    assertEquals(-1, reader.read());
  }

  @Test
  public void testPrologCharDataSourceReadableByteChannel() throws Exception {
    assertThrows(NullPointerException.class, () -> new PrologCharDataSource((ReadableByteChannel) null));

    final String testString = "It's a test string for prolog test. also there is UTF Привет";
    final ByteArrayInputStream inStream = new ByteArrayInputStream(testString.getBytes(Charset.forName("UTF8")));
    final PrologCharDataSource reader = new PrologCharDataSource(Channels.newChannel(inStream));

    for (final char chr : testString.toCharArray()) {
      assertEquals((int) chr, reader.read());
    }
    assertEquals(-1, reader.read());
  }

  @Test
  public void testPrologCharDataSourceReader() throws Exception {
    assertThrows(NullPointerException.class, () -> new PrologCharDataSource((Reader) null));

    final String testString = "It's a test string for prolog test. also there is UTF Привет";
    final Reader inStream = new StringReader(testString);
    final PrologCharDataSource reader = new PrologCharDataSource(inStream);

    for (final char chr : testString.toCharArray()) {
      assertEquals((int) chr, reader.read());
    }
    assertEquals(-1, reader.read());
  }

  @Test
  public void testRead() throws Exception {
    final Reader inStream = new StringReader("");
    final PrologCharDataSource reader1 = new PrologCharDataSource(inStream);
    assertEquals(-1, reader1.read());

    final Reader inStreamToClose = new StringReader("a");

    final PrologCharDataSource reader2 = new PrologCharDataSource(inStreamToClose);
    assertEquals('a', reader2.read());
    assertEquals(-1, reader2.read());

    inStreamToClose.close();

    assertThrows(IOException.class, () -> reader2.read());
  }

  @Test
  public void testCalculateDifferenceAndPushTheResultBack() throws Exception {
    Reader inStream = new StringReader("1234567890");
    PrologCharDataSource reader = new PrologCharDataSource(inStream);

    assertThrows(NullPointerException.class, () -> reader.calculateDifferenceAndPushTheResultBack(null, new FastStringBuilder("test")));
    assertThrows(NullPointerException.class, () -> reader.calculateDifferenceAndPushTheResultBack("test", null));

    for (int li = 0; li < 10; li++) {
      assertTrue(reader.read() >= 0);
    }

    assertEquals(-1, reader.read());
    assertEquals(1, reader.getLineNumber());
    assertEquals(11, reader.getNextCharStringPosition());
    reader.calculateDifferenceAndPushTheResultBack("test", new FastStringBuilder("testworld"));
    assertEquals(1, reader.getLineNumber());
    assertEquals(6, reader.getNextCharStringPosition());
    for (final char chr : "world".toCharArray()) {
      assertEquals((int) chr, reader.read());
    }
    assertEquals(-1, reader.read());

    assertEquals(1, reader.getLineNumber());
    reader.calculateDifferenceAndPushTheResultBack("test", new FastStringBuilder("test\n"));
    assertEquals(1, reader.getLineNumber());
  }

  @Test
  public void testGetPrevLineNumber() throws Exception {
    final Reader inStream = new StringReader("a\n\n\n\n\n\n\n");
    final PrologCharDataSource reader = new PrologCharDataSource(inStream);

    assertEquals(1, reader.getPrevLineNumber());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getPrevLineNumber());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getPrevLineNumber());
    assertTrue(reader.read() >= 0);
    assertEquals(2, reader.getPrevLineNumber());
    assertTrue(reader.read() >= 0);
    assertEquals(3, reader.getPrevLineNumber());
  }

  @Test
  public void testGetPreviousNextCharStringPosition() throws Exception {
    final Reader inStream = new StringReader("a\n\n\n\n\n\n\n");
    final PrologCharDataSource reader = new PrologCharDataSource(inStream);

    assertEquals(1, reader.getPreviousNextCharStringPosition());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getPreviousNextCharStringPosition());
    assertTrue(reader.read() >= 0);
    assertEquals(2, reader.getPreviousNextCharStringPosition());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getPreviousNextCharStringPosition());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getPreviousNextCharStringPosition());
  }

  @Test
  public void testGetLineNumber() throws Exception {
    Reader inStream = new StringReader("a\n\n\n\n\n\n\n");
    PrologCharDataSource reader = new PrologCharDataSource(inStream);
    assertEquals(1, reader.getLineNumber());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getLineNumber());
    assertTrue(reader.read() >= 0);
    assertEquals(2, reader.getLineNumber());
    assertTrue(reader.read() >= 0);
    assertEquals(3, reader.getLineNumber());
  }

  @Test
  public void testGetNextCharStringPosition() throws Exception {
    Reader inStream = new StringReader("12\n67890");
    PrologCharDataSource reader = new PrologCharDataSource(inStream);
    assertEquals(1, reader.getNextCharStringPosition());
    assertTrue(reader.read() >= 0);
    assertEquals(2, reader.getNextCharStringPosition());
    assertTrue(reader.read() >= 0);
    assertEquals(3, reader.getNextCharStringPosition());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getNextCharStringPosition());
  }

  @Test
  public void testPushCharBack() throws Exception {
    Reader inStream = new StringReader("12345\n67890");
    PrologCharDataSource reader = new PrologCharDataSource(inStream);
    for (int li = 0; li < 11; li++) {
      assertTrue(reader.read() >= 0);
    }
    assertEquals(2, reader.getLineNumber());
    assertEquals(6, reader.getNextCharStringPosition());
    reader.pushCharBack('o');
    reader.pushCharBack('\n');
    reader.pushCharBack('l');
    reader.pushCharBack('e');
    reader.pushCharBack('h');
    assertEquals(1, reader.getLineNumber());
    assertEquals(1, reader.getNextCharStringPosition());
    for (char chr : "hel\no".toCharArray()) {
      assertEquals(chr, (char) reader.read());
    }
    assertEquals(-1, reader.read());

    reader.pushCharBack('\n');
    reader.pushCharBack('\n');
    reader.pushCharBack('\n');
    reader.pushCharBack('\n');
    reader.pushCharBack('\n');
    assertEquals(1, reader.getLineNumber());
  }

  @Test
  public void testClose() throws Exception {
    Reader inStream = new StringReader("1234567890");
    PrologCharDataSource reader = new PrologCharDataSource(inStream);

    reader.close();

    assertThrows(IOException.class, () -> reader.read());

    reader.close();
    reader.close();
  }
}
