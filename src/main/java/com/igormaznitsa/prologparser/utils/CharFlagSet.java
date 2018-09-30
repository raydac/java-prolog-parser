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

import java.util.Arrays;

/**
 * The class allows to keep packed boolean flags in memory, addressed by a char
 * code point.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public final class CharFlagSet {

  /**
   * Inside array keeps packed boolean flags as integer values.
   */
  private final int[] charFlagMap = new int[0x800];

  /**
   * The Constructor.
   */
  public CharFlagSet() {
  }

  /**
   * Set the char in the set.
   *
   * @param chr a char
   */
  public void addChar(final char chr) {
    final int code = (int) chr;
    final int address = code >>> 5;
    final int pos = code & 0x1F;

    this.charFlagMap[address] |= (1 << pos);
  }

  /**
   * Check that a char is in the set.
   *
   * @param chr a char to be checked.
   * @return true if the char is presented in the set, false otherwise.
   */
  public boolean containsChar(final char chr) {
    final int code = (int) chr;
    final int address = code >>> 5;
    final int pos = code & 0x1F;

    return (this.charFlagMap[address] & (0x1 << pos)) != 0;
  }

  /**
   * Clear the set.
   */
  public void clear() {
    Arrays.fill(this.charFlagMap, 0);
  }
}
