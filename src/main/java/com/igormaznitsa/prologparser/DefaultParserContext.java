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

import static java.util.stream.IntStream.rangeClosed;

import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.tokenizer.Op;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Default implementation of parser context.
 * <b>It is not thread safe one!</b>
 */
@SuppressWarnings("unused")
public class DefaultParserContext implements ParserContext {

  protected static final Op[] EMPTY = new Op[0];
  protected final Set<String> opPrefixes = new HashSet<>();
  protected final Map<String, OpContainer> opContainers = new HashMap<>();
  protected final int parserContextFlags;

  public DefaultParserContext(final int parserContextFlags) {
    this.parserContextFlags = parserContextFlags;
  }

  public static ParserContext of(final int parserContextFlags) {
    return new DefaultParserContext(parserContextFlags);
  }

  public static ParserContext of(final int parserContextFlags, final Op... operators) {
    return new DefaultParserContext(parserContextFlags).addOps(operators);
  }

  @SafeVarargs
  @SuppressWarnings("varagrs")
  public static ParserContext of(final int parserContextFlags, final List<Op>... operators) {
    return new DefaultParserContext(parserContextFlags).addOps(operators);
  }

  public Map<String, OpContainer> findAllOperators() {
    return Collections.unmodifiableMap(this.opContainers);
  }

  @Override
  public int getFlags() {
    return this.parserContextFlags;
  }

  protected void fillPrefixes(final String name) {
    rangeClosed(1, name.length()).mapToObj(i -> name.substring(0, i)).forEach(this.opPrefixes::add);
  }

  @SafeVarargs
  @SuppressWarnings("varargs")
  public final DefaultParserContext addOps(final List<Op>... operators) {
    Stream.of(operators).filter(Objects::nonNull).forEach(ops -> this.addOps(ops.toArray(EMPTY)));
    return this;
  }

  public DefaultParserContext addOps(final Op... operators) {
    Stream.of(operators)
        .filter(Objects::nonNull)
        .flatMap(Op::streamOp)
        .forEach(x -> {
          fillPrefixes(x.getText());
          OpContainer container = this.opContainers.get(x.getText());
          if (container == null) {
            container = OpContainer.make(x);
            this.opContainers.put(x.getText(), container);
          } else {
            container.add(x);
          }
        });
    return this;
  }

  @Override
  public boolean hasOpStartsWith(final PrologParser source,
                                 final String operatorNameStartSubstring) {
    return this.opPrefixes.contains(operatorNameStartSubstring);
  }

  @Override
  public OpContainer findOpForName(final PrologParser source, final String operatorName) {
    return this.opContainers.get(operatorName);
  }
}
