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
package com.igormaznitsa.prologparser.utils;

/**
 * A String builder working a bit faster than the standard one for the parsing
 * purposes.
 *
 * @author Igor Maznita (http://www.igormaznitsa.com)
 */
public final class FastStringBuilder {

  /**
   * Inside char buffer.
   */
  private char[] charArray;
  /**
   * The pointer to the first free position in the buffer.
   */
  private int pointer;
  /**
   * The maximum position for the pointer in the buffer.
   */
  private int maxPosition;

  /**
   * A Constructor.
   *
   * @param initialString an initial string to create the buffer and to be
   * placed in the start.
   */
  public FastStringBuilder(final String initialString) {
    this(Math.max(initialString.length() << 1, 32));
    append(initialString);
  }

  /**
   * A Constructor.
   *
   * @param capacity a capacity of the buffer.
   */
  public FastStringBuilder(final int capacity) {
    this.charArray = new char[capacity];
    this.maxPosition = capacity - 1;
  }

  /**
   * Inside method to double the buffer capacity.
   */
  private void doubleBuffer() {
    final int newlen = this.charArray.length << 1;
    final char[] newbuffer = new char[newlen];
    System.arraycopy(this.charArray, 0, newbuffer, 0, this.pointer);
    this.charArray = newbuffer;
    this.maxPosition = newlen - 1;
  }

  /**
   * Append a char in the first free position.
   *
   * @param chr a char to be appended.
   * @return the buffer instance.
   */
  public FastStringBuilder append(final char chr) {
    if (this.pointer == this.maxPosition) {
      doubleBuffer();
    }
    this.charArray[this.pointer++] = chr;
    return this;
  }

  /**
   * Get the length of the string represented by the buffer.
   *
   * @return the length as integer.
   */
  public int length() {
    return this.pointer;
  }

  /**
   * Get a char at a position in the inside buffer. The method doesn't check the
   * position value!
   *
   * @param position the char position.
   * @return a characted found in the position.
   */
  public char charAt(final int position) {
    return this.charArray[position];
  }

  /**
   * Find the first position of a char in the buffer.
   *
   * @param chr a char to be searched.
   * @return -1 if the char is not found, its first position otherwise
   */
  public int indexOf(final char chr) {
    final char[] local = this.charArray;
    final int localp = this.pointer;
    for (int i = 0; i < localp; i++) {
      if (local[i] == chr) {
        return i;
      }
    }
    return -1;
  }

  public int lastIndexOf(final char chr) {
    final char[] local = this.charArray;
    int pos = this.pointer;
    while (pos != 0) {
      if (local[--pos] == chr) {
        return pos;
      }
    }
    return -1;
  }

  /**
   * Check that the buffer has a single char, its length is 1.
   *
   * @param chr a char to be checked for its presentation in the buffer.
   * @return true if the buffer has only the character as its content, false
   * otherwise.
   */
  public boolean hasSingleChar(final char chr) {
    return this.pointer == 1 && this.charArray[0] == chr;
  }

  /**
   * Check that a char is the last char in the buffer.
   *
   * @param chr a char to be checked.
   * @return false if the end of the buffer is not the char, true otherwise.
   */
  public boolean isLastChar(final char chr) {
    final int p = this.pointer;
    return p != 0 && this.charArray[p - 1] == chr;
  }

  /**
   * Make a string from the buffer exclude the last saved char.
   *
   * @return a String from the buffer, without the last char.
   * @throws IndexOutOfBoundsException if the buffer is empty.
   */
  public String toStringExcludeLastChar() {
    final int p = this.pointer;
    if (p == 0) {
      throw new IndexOutOfBoundsException("The buffer is empty");
    }
    final int len = p - 1;
    final char[] array = new char[len];
    System.arraycopy(this.charArray, 0, array, 0, len);
    return new String(array);
  }

  /**
   * Get a substring from the buffer.
   *
   * @param offset the offset to the first char of the substing in the buffer.
   * @param count the count of chars in the substring.
   * @return a substing from the buffer.
   * @throws IndexOutOfBoundsException if the substring end position greater
   * then the buffer length.
   */
  public String substring(final int offset, final int count) {
    final int maxpos = offset + count;
    if (maxpos > this.pointer) {
      throw new IndexOutOfBoundsException("The end of the substring is out of bound of the inside buffer [" + maxpos + ']');
    }
    return new String(this.charArray, offset, count);
  }

  /**
   * Append a string to the buffer.
   *
   * @param str a string to be added into the buffer.
   * @return the builder.
   */
  public FastStringBuilder append(final String str) {
    int strlen = str.length();
    while (this.pointer + strlen > this.maxPosition) {
      doubleBuffer();
    }

    final char[] localarray = this.charArray;

    int index = 0;
    int localpointer = this.pointer;
    while (strlen != 0) {
      localarray[localpointer++] = str.charAt(index++);
      strlen--;
    }
    this.pointer = localpointer;
    return this;
  }

  /**
   * Set the length for the buffer data.
   *
   * @param length the new length of the buffer.
   * @throws IllegalArgumentException will be thrown if the new length is
   * greater than the current length.
   */
  public void setLength(final int length) {
    if (length > this.pointer) {
      throw new IllegalArgumentException("The value is bigger than the current buffer length");
    }
    this.pointer = length;
  }

  /**
   * Clear the buffer.
   */
  public void clear() {
    this.pointer = 0;
  }

  @Override
  public String toString() {
    final int len = this.pointer;
    final char[] array = new char[len];
    System.arraycopy(this.charArray, 0, array, 0, len);
    return new String(array);
  }

  /**
   * Add a char in the end of the buffer.
   *
   * @param chr a char to be added.
   */
  public void pushChar(final char chr) {
    if (this.pointer == this.maxPosition) {
      doubleBuffer();
    }
    this.charArray[this.pointer++] = chr;
  }

  /**
   * Pop the last char from the buffer.
   *
   * @return a char found in the end of the buffer.
   */
  public char popChar() {
    return this.charArray[--this.pointer];
  }

  /**
   * Check that the buffer is empty one.
   *
   * @return true if the buffer is empty, false otherwise.
   */
  public boolean isEmpty() {
    return this.pointer == 0;
  }
}
