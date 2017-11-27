/*
 * Copyright 2015 Igor Maznitsa.
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

import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

public class AssertTest {
  @Test
  public void testAssertCanCast(){
    Assert.assertCanCast("HHH", Number.class, 123);
    
    final String text = "TEST";
    try{
      Assert.assertCanCast(text, Number.class, "Hello");
      fail("Must throw IAE");
    }catch(IllegalArgumentException ex){
      assertSame(text, ex.getMessage());
    }
  }

  @Test
  public void testAssertNotNull(){
    Assert.assertNotNull("HHH", "NotNull");
    
    final String text = "TEST";
    try{
      Assert.assertNotNull(text, null);
      fail("Must throw NPE");
    }catch(NullPointerException ex){
      assertSame(text, ex.getMessage());
    }
  }

  @Test
  public void testAssertNonEmptyString(){
    Assert.assertNonEmptyString("HHH", "NonEmpty");
    
    final String text = "TEST";
    try{
      Assert.assertNonEmptyString(text, "");
      fail("Must throw IAE");
    }catch(IllegalArgumentException ex){
      assertSame(text, ex.getMessage());
    }
  }

  @Test
  public void testAssertArrayDoesntContanNull(){
    Assert.assertArrayDoesntContanNull("HHH", new Object[]{"1","2"});
    
    final String text = "TEST";
    try{
      Assert.assertArrayDoesntContanNull(text, new Object[]{"1", "2", null});
      fail("Must throw NPE");
    }catch(NullPointerException ex){
      assertSame(text, ex.getMessage());
    }
  }
}
