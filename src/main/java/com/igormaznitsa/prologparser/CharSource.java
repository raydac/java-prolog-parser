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

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.Charset;

/**
 * Source of char data for Prolog parser.
 *
 * @since 2.0.0
 */
public class CharSource implements Closeable {

  private final Reader inReader;
  private final StrBuffer insideCharBuffer = new StrBuffer(32);
  private int prevStrPos;
  private int prevLineNum;
  private int strPos;
  private int lineNum;

  protected CharSource(final Reader reader) {
    if (reader == null) {
      throw new NullPointerException("Reader must not be null");
    }

    this.inReader = reader;
    this.strPos = 1;
    this.lineNum = 1;
    this.prevStrPos = 1;
    this.prevLineNum = 1;
  }

  public static CharSource of(final InputStream stream, final Charset charset) {
    return new CharSource(new InputStreamReader(stream, charset));
  }

  public static CharSource of(final ReadableByteChannel channel, final Charset charset) {
    return new CharSource(new InputStreamReader(Channels.newInputStream(channel), charset));
  }

  public static CharSource of(final String str) {
    return new CharSource(new StringReader(str));
  }

  public static CharSource of(final Reader reader) {
    return new CharSource(reader);
  }

  /**
   * Read next char
   *
   * @return the next char or -1 if end of stream
   * @throws IOException if problem during read
   */
  public int read() throws IOException {
    int ch;
    if (this.insideCharBuffer.isEmpty()) {
      ch = this.inReader.read();
    } else {
      ch = this.insideCharBuffer.popChar();
    }

    this.prevStrPos = this.strPos;
    this.prevLineNum = this.lineNum;
    if (ch == '\n') {
      this.strPos = 1;
      this.lineNum++;
    } else {
      if (ch >= 0) {
        this.strPos++;
      }
    }
    return ch;
  }

  /**
   * Calculate difference between etalon string and a string buffer content
   * since start of string and push all difference back into internal buffer
   *
   * @param etalon etalon string must not be null
   * @param buffer string buffer, must not be null
   */
  public void calcDiffAndPushResultBack(final String etalon, final StrBuffer buffer) {
    int chars = buffer.length() - etalon.length();
    int pos = buffer.length() - 1;

    int lstrpos = this.strPos;
    int lstrposprev = this.prevStrPos;
    int llinenum = this.lineNum;
    int llinenumprev = this.prevLineNum;

    while (chars > 0) {
      final char ch = buffer.charAt(pos--);
      this.insideCharBuffer.pushChar(ch);
      chars--;
      lstrpos--;
      if (lstrpos < 1) {
        lstrpos = 1;
      }
      lstrposprev = lstrpos;
      if (ch == '\n') {
        llinenum--;
        if (llinenum < 1) {
          llinenum = 1;
        }
        llinenumprev = llinenum;
      }
    }

    this.strPos = lstrpos;
    this.prevStrPos = lstrposprev;
    this.lineNum = llinenum;
    this.prevLineNum = llinenumprev;
  }

  /**
   * Get previous line number, the first line is 1
   *
   * @return previous line number
   */
  public int getPrevLineNum() {
    return this.prevLineNum;
  }

  /**
   * Get previous value of string position indicator, the
   * first char is 1
   *
   * @return previous value of string position
   */
  public int getPrevStrPos() {
    return this.prevStrPos;
  }

  /**
   * Get current line number, the first line is 1
   *
   * @return line number as integer
   */
  public int getLineNum() {
    return this.lineNum;
  }

  /**
   * Get current string position, the first char is 1
   *
   * @return the string position as integer
   */
  public int getStrPos() {
    return this.strPos;
  }

  /**
   * Push back a char into inside buffer
   *
   * @param ch the char to be pushed back into inside buffer
   */
  public void push(final char ch) {
    this.insideCharBuffer.pushChar(ch);
    if (ch == '\n') {
      this.strPos = 1;
      this.lineNum--;
      if (this.lineNum <= 0) {
        this.lineNum = 1;
      }
    } else {
      this.strPos--;
      if (this.strPos <= 0) {
        this.strPos = 1;
      }
    }
  }

  @Override
  public void close() throws IOException {
    this.inReader.close();
  }
}
