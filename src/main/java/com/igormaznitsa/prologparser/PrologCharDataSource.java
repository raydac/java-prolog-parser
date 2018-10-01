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

import com.igormaznitsa.prologparser.utils.StrBuffer;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.Charset;

/**
 * The class is the main char data source for a prolog parser, the class adapts
 * different standard Java input stream classes to be used by a prolog parser.
 * The class is not thread safe so it must not be simultaneously used from
 * different threads.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public class PrologCharDataSource {

  /**
   * The text reader which is being used by the reader to read incoming text
   * data
   */
  private final Reader inReader;
  /**
   * Inside char stack to save back-pushed data
   */
  private final StrBuffer insideCharBuffer = new StrBuffer(16);
  /**
   * The variable contains the previous value of the string position indicator
   */
  private int strPosPrev;
  /**
   * The variable contains the previous position of the line number indicator
   */
  private int lineNumPrev;
  /**
   * The variable contains current value of the string position indicator
   */
  private int strPos;
  /**
   * The variable contains current value of the line number indicator
   */
  private int lineNum;

  /**
   * A constructor. To make a reader based on a String object.
   *
   * @param string A string object which will be used as the source for the
   *               reader, must not be null
   */
  public PrologCharDataSource(final String string) {
    this(new StringReader(string));
  }

  /**
   * A constructor. To make a reader based on an input stream (with the
   * default charset!)
   *
   * @param inStream an input stream object which will be used as the source
   *                 for the reader, must not be null
   */
  public PrologCharDataSource(final InputStream inStream) {
    this(new InputStreamReader(inStream, Charset.defaultCharset()));
  }

  /**
   * A constructor. To make a reader based on a channel
   *
   * @param channel the channel to be used as the data source, must not be
   *                null
   */
  public PrologCharDataSource(final ReadableByteChannel channel) {
    this(Channels.newInputStream(channel));
  }

  /**
   * A constructor. To make a reader based on a java reader object.
   *
   * @param reader a java reader object, must not be null
   */
  public PrologCharDataSource(final Reader reader) {
    if (reader == null) {
      throw new NullPointerException("Reader must not be null");
    }

    inReader = reader;
    strPos = 1;
    lineNum = 1;
    strPosPrev = strPos;
    lineNumPrev = lineNum;
  }

  /**
   * Read next char code from the reader
   *
   * @return the next char code or -1 if the stream end has been reached
   * @throws IOException it will be thrown if there is any transport error
   *                     during the operation
   */
  public int read() throws IOException {
    int ch;
    if (insideCharBuffer.isEmpty()) {
      ch = inReader.read();
    } else {
      ch = insideCharBuffer.popChar();
    }

    strPosPrev = strPos;
    lineNumPrev = lineNum;
    if (ch == '\n') {
      strPos = 1;
      lineNum++;
    } else {
      if (ch >= 0) {
        strPos++;
      }
    }
    return ch;
  }

  /**
   * Push back the difference between an etalon string and a string buffer
   * content. For instance if there are 'test' as the etalon and a string
   * buffer containing 'testhello' then part 'hello' will be pushed back into
   * inside buffer to be used in next read operations.
   *
   * @param etalon an etalon string must not be null
   * @param buffer a string buffer object, must not be null
   */
  public void calcDiffAndPushResultBack(final String etalon, final StrBuffer buffer) {
    int chars = buffer.length() - etalon.length();
    int pos = buffer.length() - 1;

    int locStrPos = strPos;
    int locStrPosPrev = strPosPrev;
    int locLineNum = lineNum;
    int locLineNumPrev = lineNumPrev;

    while (chars > 0) {
      final char ch = buffer.charAt(pos--);
      this.insideCharBuffer.pushChar(ch);
      chars--;
      locStrPos--;
      if (locStrPos < 1) {
        locStrPos = 1;
      }
      locStrPosPrev = locStrPos;
      if (ch == '\n') {
        locLineNum--;
        if (locLineNum < 1) {
          locLineNum = 1;
        }
        locLineNumPrev = locLineNum;
      }
    }

    strPos = locStrPos;
    strPosPrev = locStrPosPrev;
    lineNum = locLineNum;
    lineNumPrev = locLineNumPrev;
  }

  /**
   * Get the previous line number, the first line is 1
   *
   * @return the previous line number
   */
  public int getPrevLineNumber() {
    return lineNumPrev;
  }

  /**
   * Get the previous value of the next char string position indicator, the
   * first char is 1
   *
   * @return the previous value of the next char string position
   */
  public int getPreviousNextCharStringPosition() {
    return strPosPrev;
  }

  /**
   * Get current line number, the first line is 1
   *
   * @return the line number as integer
   */
  public int getLineNumber() {
    return lineNum;
  }

  /**
   * Get current next char string position, the first char is 1
   *
   * @return the next char string position as integer
   */
  public int getNextCharStringPosition() {
    return strPos;
  }

  /**
   * Push a char back into the inside buffer to be read into the next read
   * operation
   *
   * @param ch the char to be placed into the inside buffer
   */
  public void pushBack(final char ch) {
    insideCharBuffer.pushChar(ch);
    if (ch == '\n') {
      strPos = 1;
      lineNum--;
      if (lineNum <= 0) {
        lineNum = 1;
      }
    } else {
      strPos--;
      if (strPos <= 0) {
        strPos = 1;
      }
    }
  }

  /**
   * Close the current reader
   *
   * @throws IOException it will be thrown if there is any error during the
   *                     operation
   */
  public void close() throws IOException {
    inReader.close();
  }
}
