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
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpType;
import com.igormaznitsa.prologparser.tokenizer.PrologParser;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

public class DefaultParserContext implements ParserContext {

  public static final Op OPERATORS_PLUS_MINUS = Op.make(200, OpType.FY, "-", "+");

  protected final Set<String> opPrefixes = new HashSet<>();
  protected final Map<String, OpContainer> opContainers = new HashMap<>();
  protected final Set<String> zeroArityStructs = new HashSet<>();

  protected final int flags;

  public DefaultParserContext(final int flags, final Op... operators) {
    this.flags = flags;
    this.addOperators(operators);
  }

  @Override
  public Map<String, OpContainer> findAllOperators() {
    return Collections.unmodifiableMap(this.opContainers);
  }

  @Override
  public int getFlags() {
    return this.flags;
  }

  protected void fillPrefixes(final String name) {
    for (int i = 1; i <= name.length(); i++) {
      this.opPrefixes.add(name.substring(0, i));
    }
  }

  public DefaultParserContext addZeroArityStructs(final String... names) {
    for (final String name : names) {
      this.zeroArityStructs.add(name);
    }
    return this;
  }

  public DefaultParserContext addOperators(final Op... operators) {
    Stream.of(operators).flatMap(x -> x.streamOp()).forEach(x -> {
      fillPrefixes(x.getTermText());
      OpContainer container = this.opContainers.get(x.getTermText());
      if (container == null) {
        container = OpContainer.make(x);
        this.opContainers.put(x.getTermText(), container);
      } else {
        container.add(x);
      }
    });
    return this;
  }

  @Override
  public void onStructureCreated(final PrologParser source, final PrologStruct struct) {
  }

  @Override
  public boolean hasZeroArityStruct(final PrologParser source, String atomName) {
    return this.zeroArityStructs.contains(atomName);
  }

  @Override
  public boolean hasOperatorStartsWith(final PrologParser source, String operatorNameStartSubstring) {
    return this.opPrefixes.contains(operatorNameStartSubstring);
  }

  @Override
  public OpContainer findOperatorForName(final PrologParser source, String operatorName) {
    return this.opContainers.get(operatorName);
  }
}
