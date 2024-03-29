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

import static java.util.Arrays.stream;
import static java.util.Objects.requireNonNull;

import com.igormaznitsa.prologparser.terms.OpContainer;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * Auxiliary class allows to make chain of prolog contexts.
 */
@SuppressWarnings("unused")
public class ParserContextChain implements ParserContext {
  private final ParserContext[] contexts;
  private final int tokenizerFlags;
  private final int minDetectedAllowedBufferSize;

  public ParserContextChain(final ParserContext... contexts) {
    this.contexts = stream(contexts).filter(Objects::nonNull).toArray(ParserContext[]::new);
    this.minDetectedAllowedBufferSize =
        Stream.of(this.contexts).mapToInt(ParserContext::getMaxTokenizerBufferLength).min()
            .orElse(Integer.MAX_VALUE);
    this.tokenizerFlags = stream(this.contexts)
        .mapToInt(ParserContext::getFlags)
        .reduce(FLAG_NONE, (a, b) -> a | b);
  }

  public static ParserContext of(final ParserContext... contexts) {
    final ParserContext result;
    if (contexts.length == 1) {
      result = requireNonNull(contexts[0]);
    } else {
      result = new ParserContextChain(contexts);
    }
    return result;
  }

  @Override
  public int getMaxTokenizerBufferLength() {
    return this.minDetectedAllowedBufferSize;
  }

  @Override
  public boolean hasOpStartsWith(final PrologParser source, final String namePrefix) {
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
  public OpContainer findOpForName(final PrologParser source, final String name) {
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
  public int getFlags() {
    return this.tokenizerFlags;
  }
}
