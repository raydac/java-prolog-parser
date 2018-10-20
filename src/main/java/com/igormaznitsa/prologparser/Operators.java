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

package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.tokenizer.Op;

import static com.igormaznitsa.prologparser.tokenizer.Op.make;
import static com.igormaznitsa.prologparser.tokenizer.OpType.*;

public final class Operators {
  public static final Op UNARY_PLUS_MINUS = make(200, FY, "+", "-");
  public static final Op BINARY_PLUS_MINUS = make(500, YFX, "+", "-");
  public static final Op BINARY_MUL_DIV = make(400, YFX, "*", "/");
  public static final Op UNIFY = make(700, XFX, "=");
  public static final Op CONDITIONAL = make(700, XFX, "==", "<", "=<", ">", ">=");
  public static final Op BITWISE_SHIFT = make(400, YFX, "<<", ">>");
  public static final Op[] SWI_CPL = {
      make(760, YFX, "#<==>"),
      make(750, XFY, "#==>"),
      make(750, YFX, "#<=="),
      make(740, YFX, "#\\/"),
      make(730, YFX, "#\\"),
      make(720, YFX, "#/\\"),
      make(710, FY, "#\\"),
      make(700, XFX, "#>", "#<", "#>=", "#=<", "#=", "#\\=", "in", "ins"),
      make(450, XFX, ".."),
  };

  private Operators() {
  }
}
