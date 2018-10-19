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

import static com.igormaznitsa.prologparser.ParserContext.FLAG_BLOCK_COMMENTS;
import static com.igormaznitsa.prologparser.ParserContexts.of;
import static com.igormaznitsa.prologparser.operators.Ops.of;

public class EdinburghPrologParser extends GenericPrologParser {

  public EdinburghPrologParser(final Reader reader, final ParserContext context) {
    super(reader, of(new DefaultParserContext(FLAG_BLOCK_COMMENTS,
        of(1200, OpType.XFX, ":-", "-->"),
        of(1200, OpType.FX, "?-", ":-"),
        of(1100, OpType.XFY, ";"),
        of(1150, OpType.XFY, "->"),
        of(900, OpType.FY, "\\+"),
        of(700, OpType.XFX, "=", "\\=", "==", "\\==", "@<", "@=<", "@>", "@>+", "=..", "is", "=:=", "=\\=", "<", "=<", ">", ">="),
        of(500, OpType.YFX, "+", "-", "/\\", "\\/"),
        of(400, OpType.YFX, "*", "/", "//", "<<", ">>", "rem", "mod"),
        of(200, OpType.XFX, "**"),
        of(200, OpType.XFY, "^"),
        of(200, OpType.FY, "-", "+", "\\")
    ), context));
  }

  public EdinburghPrologParser(final Reader reader) {
    this(reader, null);
  }
}
