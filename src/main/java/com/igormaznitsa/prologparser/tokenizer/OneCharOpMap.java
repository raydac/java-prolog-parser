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

package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.terms.OpContainer;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static java.util.Arrays.stream;

final class OneCharOpMap {

  private final Map<String, OpContainer> insideMap = new HashMap<>();
  private final Map<String, OpContainer> unmodifableInsideMap = Collections.unmodifiableMap(this.insideMap);
  private final OpContainer[] charMap = new OpContainer[0x80];

  OneCharOpMap() {
  }

  OneCharOpMap(final OpContainer... containers) {
    stream(containers).forEach(x -> put(x.getTermText(), x));
  }

  boolean containsKey(final String key) {
    if (key.length() != 1) {
      return false;
    }
    final int chr = key.charAt(0);
    return chr < 0x80 && charMap[chr] != null;
  }

  void put(final String key, final OpContainer container) {
    if (key.length() != 1) {
      throw new IllegalArgumentException("Wrong key [" + key + ']');
    }

    final int chr = key.charAt(0);
    if (chr > 0x7F) {
      throw new IllegalArgumentException("The char code is greater than 0x7F");
    }

    charMap[chr] = container;
    insideMap.put(key, container);
  }

  OpContainer get(final String key) {
    if (key.length() != 1) {
      return null;
    }

    final int code = key.charAt(0);

    if (code > 0x7F) {
      return null;
    }
    return charMap[code];
  }

  OpContainer get(final char c) {
    return c > 0x7F ? null : charMap[c];
  }

  Map<String, OpContainer> getUnmodifableMap() {
    return this.unmodifableInsideMap;
  }

  void clear() {
    insideMap.clear();
    Arrays.fill(charMap, null);
  }
}
