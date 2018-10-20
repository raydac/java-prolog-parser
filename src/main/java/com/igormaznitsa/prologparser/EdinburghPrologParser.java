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

import com.igormaznitsa.prologparser.tokenizer.OpType;

import java.io.Reader;

import static com.igormaznitsa.prologparser.ParserContext.FLAG_BLOCK_COMMENTS;
import static com.igormaznitsa.prologparser.ParserContexts.of;
import static com.igormaznitsa.prologparser.tokenizer.Op.make;

public class EdinburghPrologParser extends GenericPrologParser {

  public EdinburghPrologParser(final Reader reader, final ParserContext context) {
    super(reader, of(new DefaultParserContext(FLAG_BLOCK_COMMENTS,
        make(1200, OpType.XFX, ":-", "-->"),
        make(1200, OpType.FX, "?-", ":-"),
        make(1150, OpType.FX, "dynamic", "discontiguous", "initialization", "meta_predicate", "module_transparent", "multifile", "public", "thread_local", "thread_initialization", "volatile"),
        make(1100, OpType.XFY, ";"),
        make(1050, OpType.XFY, "->", "*->"),
        make(990, OpType.FY, ":="),
        make(900, OpType.FY, "\\+"),
        Operators.UNIFY,
        Operators.CONDITIONAL,
        make(700, OpType.XFX, "=..", "=@=", "\\=@=", "=:=", "=\\=", "@<", "@=<", "@>", "@>=", "\\=", "\\==", "as", "is", ">:<", ":<"),
        make(600, OpType.XFY, ":"),
        Operators.BINARY_PLUS_MINUS,
        make(500, OpType.YFX, "/\\", "\\/", "xor"),
        make(500, OpType.FX, "?"),
        Operators.BINARY_MUL_DIV,
        Operators.BITWISE_SHIFT,
        make(400, OpType.YFX, "//", "div", "rdiv", "mod", "rem"),
        make(200, OpType.XFX, "**"),
        make(200, OpType.XFY, "^"),
        Operators.UNARY_PLUS_MINUS,
        make(200, OpType.FY, "\\"),
        make(1, OpType.FX, "$")
    ), context));
  }

  public EdinburghPrologParser(final Reader reader) {
    this(reader, null);
  }
}
