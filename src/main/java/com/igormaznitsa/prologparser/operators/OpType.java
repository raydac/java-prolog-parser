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

import java.util.Optional;
import java.util.stream.Stream;

public enum OpType {

  XF("xf", 1),
  YF("yf", 1),
  FX("fx", 1),
  FY("fy", 1),
  XFX("xfx", 2),
  XFY("xfy", 2),
  YFX("yfx", 2);

  private final String text;

  private final int arity;

  OpType(final String text, final int arity) {
    this.text = text;
    this.arity = arity;
  }

  public static Optional<OpType> getForName(final String str) {
    return Stream.of(values()).filter(x -> x.text.equals(str)).findFirst();
  }

  public int getArity() {
    return this.arity;
  }

  public String getText() {
    return text;
  }
}
