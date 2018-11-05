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

import com.igormaznitsa.prologparser.terms.OpContainer;

import java.util.Map;

public interface ParserContext {
  /**
   * Empty flags, no any flag defined.
   */
  int FLAG_NONE = 0;
  /**
   * Flag allows block comments.
   */
  int FLAG_BLOCK_COMMENTS = 1;
  /**
   * Flag allows construction 0'{char} to get char code.
   * Example: 0'a, 0'\r
   */
  int FLAG_ZERO_QUOTATION_CHARCODE = 2;
  /**
   * Flag allows variable as structure functor.
   * Example: Var(a,b,c).
   */
  int FLAG_VAR_AS_FUNCTOR = 4;
  /**
   * Flag allows zero-arity structures.
   * Example: a().
   */
  int FLAG_ZERO_STRUCT = 8;
  /**
   * Flag allows blocks bounded by curly brackets.
   * Example: {a,{b,!,c}}
   */
  int FLAG_CURLY_BRACKETS = 16;

  boolean hasOpStartsWith(PrologParser source, String namePrefix);

  OpContainer findOpForName(PrologParser source, String name);

  Map<String, OpContainer> findAllOperators();

  default int getMaxTokenizerBufferLength() {
    return Integer.MAX_VALUE;
  }

  int getFlags();
}
