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

package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.utils.AssertUtils;

import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.unmodifiableList;

public final class OpDef {

  private final int precedence;
  private final OpType type;
  private final List<String> names;

  private OpDef(final int precedence, final OpType type, final String... names) {
    AssertUtils.assertNotNull(type);
    if (precedence < 0 || precedence > 1200) {
      throw new IllegalArgumentException("Precedence must be in 0..1200");
    }
    this.precedence = precedence;
    this.type = type;
    this.names = unmodifiableList(asList(names));
  }

  public static OpDef op(final int precedence, final OpType type, final String... names) {
    return new OpDef(precedence, type, names);
  }

  public int getPrecedence() {
    return this.precedence;
  }

  public List<String> getNames() {
    return this.names;
  }

  public OpType getType() {
    return this.type;
  }
}
