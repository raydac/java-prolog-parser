package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.utils.StrBuffer;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;

public class PrologCharDataSourceTest {

  @Test
  public void testPrologCharDataSourceString() throws Exception {
    assertThrows(NullPointerException.class, () -> CharSource.of((String) null));

    final String testString = "It's a test string for prolog test. also there is UTF Привет";
    final CharSource reader = CharSource.of(testString);
    for (final char chr : testString.toCharArray()) {
      assertEquals((int) chr, reader.read());
    }
    assertEquals(-1, reader.read());
  }

  @Test
  public void testPrologCharDataSourceReadableByteChannel() throws Exception {
    assertThrows(NullPointerException.class, () -> CharSource.of((ReadableByteChannel) null, StandardCharsets.US_ASCII));

    final String testString = "It's a test string for prolog test. also there is UTF Привет";
    final ByteArrayInputStream inStream = new ByteArrayInputStream(testString.getBytes(StandardCharsets.UTF_8));
    final CharSource reader = CharSource.of(Channels.newChannel(inStream), StandardCharsets.UTF_8);

    for (final char chr : testString.toCharArray()) {
      assertEquals((int) chr, reader.read());
    }
    assertEquals(-1, reader.read());
  }

  @Test
  public void testPrologCharDataSourceReader() throws Exception {
    assertThrows(NullPointerException.class, () -> new CharSource((Reader) null));

    final String testString = "It's a test string for prolog test. also there is UTF Привет";
    final Reader inStream = new StringReader(testString);
    final CharSource reader = new CharSource(inStream);

    for (final char chr : testString.toCharArray()) {
      assertEquals((int) chr, reader.read());
    }
    assertEquals(-1, reader.read());
  }

  @Test
  public void testRead() throws Exception {
    final Reader inStream = new StringReader("");
    final CharSource reader1 = new CharSource(inStream);
    assertEquals(-1, reader1.read());

    final Reader inStreamToClose = new StringReader("a");

    final CharSource reader2 = new CharSource(inStreamToClose);
    assertEquals('a', reader2.read());
    assertEquals(-1, reader2.read());

    inStreamToClose.close();

    assertThrows(IOException.class, reader2::read);
  }

  @Test
  public void testCalculateDifferenceAndPushTheResultBack() throws Exception {
    Reader inStream = new StringReader("1234567890");
    CharSource reader = new CharSource(inStream);

    assertThrows(NullPointerException.class, () -> reader.calcDiffAndPushResultBack(null, new StrBuffer("test")));
    assertThrows(NullPointerException.class, () -> reader.calcDiffAndPushResultBack("test", null));

    for (int li = 0; li < 10; li++) {
      assertTrue(reader.read() >= 0);
    }

    assertEquals(-1, reader.read());
    assertEquals(1, reader.getLineNum());
    assertEquals(11, reader.getStrPos());
    reader.calcDiffAndPushResultBack("test", new StrBuffer("testworld"));
    assertEquals(1, reader.getLineNum());
    assertEquals(6, reader.getStrPos());
    for (final char chr : "world".toCharArray()) {
      assertEquals((int) chr, reader.read());
    }
    assertEquals(-1, reader.read());

    assertEquals(1, reader.getLineNum());
    reader.calcDiffAndPushResultBack("test", new StrBuffer("test\n"));
    assertEquals(1, reader.getLineNum());
  }

  @Test
  public void testGetPrevLineNumber() throws Exception {
    final Reader inStream = new StringReader("a\n\n\n\n\n\n\n");
    final CharSource reader = new CharSource(inStream);

    assertEquals(1, reader.getPrevLineNum());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getPrevLineNum());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getPrevLineNum());
    assertTrue(reader.read() >= 0);
    assertEquals(2, reader.getPrevLineNum());
    assertTrue(reader.read() >= 0);
    assertEquals(3, reader.getPrevLineNum());
  }

  @Test
  public void testGetPreviousNextCharStringPosition() throws Exception {
    final Reader inStream = new StringReader("a\n\n\n\n\n\n\n");
    final CharSource reader = new CharSource(inStream);

    assertEquals(1, reader.getPrevStrPos());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getPrevStrPos());
    assertTrue(reader.read() >= 0);
    assertEquals(2, reader.getPrevStrPos());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getPrevStrPos());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getPrevStrPos());
  }

  @Test
  public void testGetLineNumber() throws Exception {
    Reader inStream = new StringReader("a\n\n\n\n\n\n\n");
    CharSource reader = new CharSource(inStream);
    assertEquals(1, reader.getLineNum());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getLineNum());
    assertTrue(reader.read() >= 0);
    assertEquals(2, reader.getLineNum());
    assertTrue(reader.read() >= 0);
    assertEquals(3, reader.getLineNum());
  }

  @Test
  public void testGetNextCharStringPosition() throws Exception {
    Reader inStream = new StringReader("12\n67890");
    CharSource reader = new CharSource(inStream);
    assertEquals(1, reader.getStrPos());
    assertTrue(reader.read() >= 0);
    assertEquals(2, reader.getStrPos());
    assertTrue(reader.read() >= 0);
    assertEquals(3, reader.getStrPos());
    assertTrue(reader.read() >= 0);
    assertEquals(1, reader.getStrPos());
  }

  @Test
  public void testPushCharBack() throws Exception {
    Reader inStream = new StringReader("12345\n67890");
    CharSource reader = new CharSource(inStream);
    for (int li = 0; li < 11; li++) {
      assertTrue(reader.read() >= 0);
    }
    assertEquals(2, reader.getLineNum());
    assertEquals(6, reader.getStrPos());
    reader.push('o');
    reader.push('\n');
    reader.push('l');
    reader.push('e');
    reader.push('h');
    assertEquals(1, reader.getLineNum());
    assertEquals(1, reader.getStrPos());
    for (char chr : "hel\no".toCharArray()) {
      assertEquals(chr, (char) reader.read());
    }
    assertEquals(-1, reader.read());

    reader.push('\n');
    reader.push('\n');
    reader.push('\n');
    reader.push('\n');
    reader.push('\n');
    assertEquals(1, reader.getLineNum());
  }

  @Test
  public void testClose() throws Exception {
    Reader inStream = new StringReader("1234567890");
    CharSource reader = new CharSource(inStream);

    reader.close();

    assertThrows(IOException.class, reader::read);

    reader.close();
    reader.close();
  }
}
