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
import com.igormaznitsa.prologparser.tokenizer.AbstractPrologParser;
import com.igormaznitsa.prologparser.utils.AssertUtils;

import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static java.util.Arrays.stream;

public class ParserContextChain implements ParserContext {
  private final ParserContext[] contexts;
  private final int flags;

  public ParserContextChain(final ParserContext... contexts) {
    this.contexts = stream(contexts).filter(Objects::nonNull).toArray(ParserContext[]::new);
    int commonFlags = stream(this.contexts).mapToInt(ParserContext::getFlags).reduce(FLAG_NONE, (a, b) -> a | b);
    this.flags = commonFlags;
  }

  public static ParserContext of(final ParserContext... contexts) {
    final ParserContext result;
    if (contexts.length == 1) {
      result = AssertUtils.assertNotNull(contexts[0]);
    } else {
      result = new ParserContextChain(contexts);
    }
    return result;
  }

  @Override
  public Map<String, OpContainer> findAllOperators() {
    return stream(this.contexts)
        .map(ParserContext::findAllOperators)
        .flatMap(x -> x.entrySet().stream())
        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
  }

  @Override
  public boolean hasOpStartsWith(final AbstractPrologParser source, final String namePrefix) {
    boolean result = false;
    for (final ParserContext c : this.contexts) {
      if (c.hasOpStartsWith(source, namePrefix)) {
        result = true;
        break;
      }
    }
    return result;
  }

  @Override
  public OpContainer findOpForName(final AbstractPrologParser source, final String name) {
    OpContainer result = null;
    for (final ParserContext c : this.contexts) {
      result = c.findOpForName(source, name);
      if (result != null) {
        break;
      }
    }
    return result;
  }

  @Override
  public boolean hasZeroStruct(final AbstractPrologParser source, final String atomName) {
    boolean result = false;
    for (final ParserContext c : this.contexts) {
      if (c.hasZeroStruct(source, atomName)) {
        result = true;
        break;
      }
    }
    return result;
  }

  @Override
  public void onNewStruct(final AbstractPrologParser source, final PrologStruct struct) {
    Arrays.stream(this.contexts).forEach(c -> c.onNewStruct(source, struct));
  }

  @Override
  public int getFlags() {
    return this.flags;
  }
}
