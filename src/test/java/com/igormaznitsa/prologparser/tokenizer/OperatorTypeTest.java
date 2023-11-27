/*
 * Copyright (c) 2011-2018 Igor Maznitsa. All rights reserved.
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package com.igormaznitsa.prologparser.tokenizer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;

@SuppressWarnings("OptionalGetWithoutIsPresent")
public class OperatorTypeTest {

  @Test
  public void testGetForName() {
    assertSame(OpAssoc.XF, OpAssoc.findForName("xf").get());
    assertSame(OpAssoc.FX, OpAssoc.findForName("fx").get());
    assertSame(OpAssoc.XFX, OpAssoc.findForName("xfx").get());
    assertSame(OpAssoc.XFY, OpAssoc.findForName("xfy").get());
    assertSame(OpAssoc.YF, OpAssoc.findForName("yf").get());
    assertSame(OpAssoc.YFX, OpAssoc.findForName("yfx").get());
    assertFalse(OpAssoc.findForName("yfy").isPresent());
  }

  @Test
  public void testGetText() {
    assertEquals("xf", OpAssoc.XF.getText());
    assertEquals("fx", OpAssoc.FX.getText());
    assertEquals("xfx", OpAssoc.XFX.getText());
    assertEquals("xfy", OpAssoc.XFY.getText());
    assertEquals("yf", OpAssoc.YF.getText());
    assertEquals("yfx", OpAssoc.YFX.getText());
  }
}
