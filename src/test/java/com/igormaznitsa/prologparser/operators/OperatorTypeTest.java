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

package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.AbstractPrologParserTest;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class OperatorTypeTest extends AbstractPrologParserTest {

  @Test
  public void testGetForName() {
    assertSame(OpType.XF, OpType.getForName("xf").get());
    assertSame(OpType.FX, OpType.getForName("fx").get());
    assertSame(OpType.XFX, OpType.getForName("xfx").get());
    assertSame(OpType.XFY, OpType.getForName("xfy").get());
    assertSame(OpType.YF, OpType.getForName("yf").get());
    assertSame(OpType.YFX, OpType.getForName("yfx").get());
    assertFalse(OpType.getForName("yfy").isPresent());
  }

  @Test
  public void testGetText() {
    assertEquals("xf", OpType.XF.getText());
    assertEquals("fx", OpType.FX.getText());
    assertEquals("xfx", OpType.XFX.getText());
    assertEquals("xfy", OpType.XFY.getText());
    assertEquals("yf", OpType.YF.getText());
    assertEquals("yfx", OpType.YFX.getText());
  }
}
