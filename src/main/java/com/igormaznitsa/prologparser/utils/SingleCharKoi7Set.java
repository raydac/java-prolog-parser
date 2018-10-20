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

package com.igormaznitsa.prologparser.utils;

public class SingleCharKoi7Set {
  private long low;
  private long high;

  public SingleCharKoi7Set() {

  }

  public long getHighMask() {
    return this.high;
  }

  public long getLowMask() {
    return this.low;
  }

  public void add(final char chr) {
    if (chr > 0x7F) {
      throw new IllegalArgumentException("Non-koi 7 char: " + chr);
    }
    if (chr < 64) {
      this.low |= (1L << chr);
    } else {
      this.high |= (1L << (chr - 64));
    }
  }

  public void clear() {
    this.low = 0L;
    this.high = 0L;
  }

  public boolean contains(final String text) {
    boolean result = false;
    if (text.length() == 1) {
      final int code = text.charAt(0);
      if (code < 0x80) {
        if (code < 64) {
          result = (this.low & (1L << code)) != 0;
        } else {
          result = (this.high & (1L << (code - 64))) != 0;
        }
      }
    }
    return result;
  }
}
