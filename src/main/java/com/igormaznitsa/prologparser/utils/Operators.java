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

package com.igormaznitsa.prologparser.utils;

import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpType;

import static com.igormaznitsa.prologparser.tokenizer.Op.make;

public final class Operators {
  public static final Op UNARY_PLUS_MINUS = make(200, OpType.FY, "+", "-");
  public static final Op BINARY_PLUS_MINUS = make(500, OpType.YFX, "+", "-");
  public static final Op BINARY_MUL_DIV = make(400, OpType.YFX, "*", "/");
  public static final Op UNIFY = make(700, OpType.XFX, "=");
  public static final Op CONDITIONAL = make(700, OpType.XFX, "==", "<", "=<", ">", ">=");
  public static final Op BITWISE_SHIFT = make(400, OpType.YFX, "<<", ">>");


  private Operators(){}
}
