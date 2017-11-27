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

import org.junit.Test;

import static org.junit.Assert.*;

public class FastStringBuilderTest {
  
  @Test
  public void testHasSingleChar() {
    final FastStringBuilder bldr = new FastStringBuilder(10);
    bldr.append('a');
    assertTrue(bldr.hasSingleChar('a'));
    assertFalse(bldr.hasSingleChar('b'));
  }
  
  @Test
  public void testIsLastChar() {
    final FastStringBuilder bldr = new FastStringBuilder(10);
    bldr.append("hello");
    assertTrue(bldr.isLastChar('o'));
    assertFalse(bldr.isLastChar('a'));
  }
  
  @Test
  public void testToStringExcludeLastChar() {
    final FastStringBuilder bldr = new FastStringBuilder(10);
    bldr.append("hello");
    assertEquals("hell",bldr.toStringExcludeLastChar());
  }
  
  @Test
  public void testSubstring() {
    final FastStringBuilder bldr = new FastStringBuilder(10);
    bldr.append("hello");
    assertEquals("ell",bldr.substring(1, 3));
  }
  
  @Test
  public void testAppend() {
    final FastStringBuilder bldr = new FastStringBuilder(3);
    bldr.append("hello");
    bldr.append("world");
    assertEquals("helloworld",bldr.toString());
  }
  
  @Test(expected = IllegalArgumentException.class)
  public void testSetLength_IAE() {
    final FastStringBuilder bldr = new FastStringBuilder(3);
    bldr.append("hello");
    assertTrue(bldr.length()>0);
    bldr.setLength(100);
  }
  
  @Test
  public void testSetLength() {
    final FastStringBuilder bldr = new FastStringBuilder(3);
    bldr.append("hello");
    bldr.setLength(3);
    assertEquals("hel", bldr.toString());    
  }
  
  @Test
  public void testClear() {
    final FastStringBuilder bldr = new FastStringBuilder(3);
    bldr.append("hello");
    bldr.clear();
    assertEquals("", bldr.toString());    
  }
  
}
