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

import com.igormaznitsa.prologparser.operators.Op;
import com.igormaznitsa.prologparser.operators.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.tokenizer.PrologParser;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class DefaultParserContext implements ParserContext {

  protected final Set<String> opPrefixes = new HashSet<>();
  protected final Map<String, OpContainer> opContainers = new HashMap<>();
  protected final Set<String> zeroArityStructs = new HashSet<>();

  public DefaultParserContext() {
  }

  protected void fillPrefixes(final String name) {
    for (int i = 1; i < name.length(); i++) {
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
    for (final Op op : operators) {
      fillPrefixes(op.getText());
      OpContainer container = this.opContainers.get(op.getText());
      if (container == null) {
        container = OpContainer.newOpCont(op);
        this.opContainers.put(op.getText(), container);
      } else {
        container.addOp(op);
      }
    }
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
