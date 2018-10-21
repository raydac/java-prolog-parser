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

import static com.igormaznitsa.prologparser.terms.OpContainer.make;
import static java.util.Arrays.stream;

/**
 * Auxiliary mapping class allows to map a single char KOI7 to its related operator data container.
 */
final class Koi7CharOpMap {

  private final OpContainer[] charMap = new OpContainer[0x80];

  private Koi7CharOpMap(final OpContainer... containers) {
    stream(containers).forEach(x -> put(x.getTermText(), x));
  }

  static Koi7CharOpMap ofOps(final OpContainer... containers) {
    return new Koi7CharOpMap(containers);
  }

  OpContainer add(final Op operator) {
    final String text = operator.getTermText();

    if (text.length() != 1) {
      throw new Error("Meta operator must be single char: " + text);
    }

    OpContainer container = this.get(text);
    if (container == null) {
      container = make(operator);
      this.put(text, container);
    }
    container.add(operator);

    return container;
  }


  boolean contains(final String key) {
    if (key.length() != 1) {
      return false;
    }
    final int chr = key.charAt(0);
    return chr < 0x80 && this.charMap[chr] != null;
  }

  void put(final String key, final OpContainer container) {
    if (key.length() != 1) {
      throw new IllegalArgumentException("Wrong key [" + key + ']');
    }

    final int chr = key.charAt(0);
    if (chr > 0x7F) {
      throw new IllegalArgumentException("The char code is greater than 0x7F");
    }

    this.charMap[chr] = container;
  }

  OpContainer get(final String key) {
    if (key.length() != 1) {
      return null;
    }

    final int code = key.charAt(0);

    final OpContainer result;
    if (code < 0x80) {
      result = this.charMap[code];
    } else {
      result = null;
    }
    return result;
  }

  OpContainer get(final char c) {
    return c > 0x7F ? null : this.charMap[c];
  }
}
