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

import com.igormaznitsa.prologparser.operators.OpType;

import java.io.Reader;

import static com.igormaznitsa.prologparser.operators.OpDef.op;

public class EdinburghPrologParser extends GenericPrologParser {

  static {
    registerSysOp(op(1200, OpType.XFX, ":-", "-->"),
        op(1200, OpType.FX, "?-", ":-"),
        op(1100, OpType.XFY, ";"),
        op(1150, OpType.XFY, "->"),
        op(900, OpType.FY, "\\+"),
        op(700, OpType.XFX, "=", "\\=", "==", "\\==", "@<", "@=<", "@>", "@>+", "=..", "is", "=:=", "=\\=", "<", "=<", ">", ">="),
        op(500, OpType.YFX, "+", "-", "/\\", "\\/"),
        op(400, OpType.YFX, "*", "/", "//", "<<", ">>", "rem", "mod"),
        op(200, OpType.XFX, "**"),
        op(200, OpType.XFY, "^"),
        op(200, OpType.FY, "-", "+", "\\"));
  }

  public EdinburghPrologParser(final Reader reader, final ParserContext context) {
    super(reader, context);
  }

  public EdinburghPrologParser(final Reader reader) {
    this(reader, null);
  }
}
