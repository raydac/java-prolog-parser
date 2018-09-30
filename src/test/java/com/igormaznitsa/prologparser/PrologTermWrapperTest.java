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

import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

public class PrologTermWrapperTest {

  final static Operator testWrapped = Operator.makeOperator(300, OperatorType.FX, "---");

  final static PrologTermWrapper testWrapper = new PrologTermWrapper();

  static {
    testWrapper.setWrappedTerm(testWrapped);
  }

  @Test
  public void testGetText() {
    assertEquals(testWrapped.getText(), testWrapper.getText());
  }

  @Test
  public void testGetPriority() {
    assertEquals(testWrapped.getPriority(), testWrapper.getPriority());
  }

  @Test
  public void testToString() {
    assertEquals(testWrapped.toString(), testWrapper.toString());
  }

  @Test
  public void testGetType() {
    assertEquals(testWrapped.getType(), testWrapper.getType());
  }

  @Test
  public void testGetWrappedTerm() {
    assertSame(testWrapped, testWrapper.getWrappedTerm());
  }
}
