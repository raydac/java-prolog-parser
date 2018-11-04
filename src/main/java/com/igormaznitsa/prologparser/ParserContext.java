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

  int FLAG_NONE = 0;
  int FLAG_BLOCK_COMMENTS = 1;
  int FLAG_ZERO_SINGLE_QUOTATION_CHAR_CODE = 2;
  int FLAG_VAR_AS_FUNCTOR = 4;
  int FLAG_ALLOW_ZERO_STRUCT = 8;

  boolean hasOpStartsWith(PrologParser source, String namePrefix);

  OpContainer findOpForName(PrologParser source, String name);

  Map<String, OpContainer> findAllOperators();

  default int getMaxTokenizerBufferLength() {
    return Integer.MAX_VALUE;
  }

  int getFlags();
}
