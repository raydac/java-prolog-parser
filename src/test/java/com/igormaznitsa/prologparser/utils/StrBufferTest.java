/*
 * Copyright 2014 Igor Maznitsa.
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

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class StrBufferTest {

  @Test
  public void testHasSingleChar() {
    final StrBuffer bldr = new StrBuffer(10);
    bldr.append('a');
    assertTrue(bldr.hasSingleChar('a'));
    assertFalse(bldr.hasSingleChar('b'));
  }

  @Test
  public void testIsLastChar() {
    final StrBuffer bldr = new StrBuffer(10);
    bldr.append("hello");
    assertTrue(bldr.isLastChar('o'));
    assertFalse(bldr.isLastChar('a'));
  }

  @Test
  public void testToStringExcludeLastChar() {
    final StrBuffer bldr = new StrBuffer(10);
    bldr.append("hello");
    assertEquals("hell", bldr.toStringExcludeLastChar());
  }

  @Test
  public void testAppend() {
    final StrBuffer bldr = new StrBuffer(3);
    bldr.append("hello");
    bldr.append("world");
    assertEquals("helloworld", bldr.toString());
  }

  @Test
  public void testClear() {
    final StrBuffer bldr = new StrBuffer(3);
    bldr.append("hello");
    bldr.clear();
    assertEquals("", bldr.toString());
  }

}
