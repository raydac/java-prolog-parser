/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.igormaznitsa.prologparser.operators;

import java.util.Optional;

import static java.util.Arrays.stream;

public enum OperatorType {

  XF("xf", 1),
  YF("yf", 1),
  FX("fx", 1),
  FY("fy", 1),
  XFX("xfx", 2),
  XFY("xfy", 2),
  YFX("yfx", 2);

  private final String text;

  private final int arity;

  OperatorType(final String text, final int arity) {
    this.text = text;
    this.arity = arity;
  }

  public static Optional<OperatorType> getForName(final String str) {
    return stream(values()).filter(x -> x.text.equals(str)).findFirst();
  }

  public int getArity() {
    return this.arity;
  }

  public String getText() {
    return text;
  }
}
