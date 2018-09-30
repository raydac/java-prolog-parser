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
    assertSame(OperatorType.XF, OperatorType.getForName("xf").get());
    assertSame(OperatorType.FX, OperatorType.getForName("fx").get());
    assertSame(OperatorType.XFX, OperatorType.getForName("xfx").get());
    assertSame(OperatorType.XFY, OperatorType.getForName("xfy").get());
    assertSame(OperatorType.YF, OperatorType.getForName("yf").get());
    assertSame(OperatorType.YFX, OperatorType.getForName("yfx").get());
    assertFalse(OperatorType.getForName("yfy").isPresent());
  }

  @Test
  public void testGetText() {
    assertEquals("xf", OperatorType.XF.getText());
    assertEquals("fx", OperatorType.FX.getText());
    assertEquals("xfx", OperatorType.XFX.getText());
    assertEquals("xfy", OperatorType.XFY.getText());
    assertEquals("yf", OperatorType.YF.getText());
    assertEquals("yfx", OperatorType.YFX.getText());
  }
}
